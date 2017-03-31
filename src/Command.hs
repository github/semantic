{-# LANGUAGE DataKinds, GADTs #-}
module Command
( module C
) where

import Command.Diff as C
import Command.Parse as C
import Control.Monad.Free.Freer
import Data.Functor.Both
import Data.List ((\\))
import Data.RandomWalkSimilarity
import Data.Record
import Data.String
import Debug.Trace (traceEventIO)
import Diff
import Info
import Interpreter
import qualified Git
import Git.Blob
import Git.Libgit2
import Git.Repository
import Git.Types
import GitmonClient
import Language
import Prologue
import Renderer
import Source
import Syntax
import Term

data CommandF f where
  ReadFile :: FilePath -> CommandF SourceBlob
  ReadFilesAtSHAs :: FilePath -> [FilePath] -> String -> String -> CommandF [(SourceBlob, SourceBlob)]

  Parse :: Language -> SourceBlob -> CommandF (Term (Syntax Text) (Record DefaultFields))

  Diff :: Term (Syntax Text) (Record DefaultFields) -> Term (Syntax Text) (Record DefaultFields) -> CommandF (Diff (Syntax Text) (Record DefaultFields))

  RenderDiff :: DiffRenderer fields output -> SourceBlob -> SourceBlob -> Diff (Syntax Text) (Record fields) -> CommandF output

  -- render a term

  -- parallelize diffs of a list of paths + git shas
  -- explicit gitmon effects/events
  -- tracing events (traceEventIO & friends) for event logs
  -- alternateObjectDirs??

type Command = Freer CommandF


-- Evaluation

runCommand :: Command a -> IO a
runCommand = iterFreerA $ \ command yield -> case command of
  ReadFile path -> do
    source <- readAndTranscodeFile path
    yield (sourceBlob source path)
  ReadFilesAtSHAs gitDir paths sha1 sha2 ->
    withRepository lgFactory gitDir $ do
      -- repo <- getRepository
      -- for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)

      liftIO $ traceEventIO ("START readFilesAtSHAs: " <> show paths)

      tree1 <- treeForSha sha1
      tree2 <- treeForSha sha2

      paths <- case paths of
        (_ : _) -> pure paths
        [] -> do
          a <- pathsForTree tree1
          b <- pathsForTree tree2

          pure $! (a \\ b) <> (b \\ a)

      blobs <- for paths $ \ path -> (,) <$> blobForPathInTree path tree1 <*> blobForPathInTree path tree2


      liftIO $! traceEventIO ("END readFilesAtSHAs: " <> show paths)
      liftIO $ yield blobs

    where treeForSha sha = do
            obj <- parseObjOid (toS sha)
            commit <- reportGitmon "cat-file" $ lookupCommit obj
            reportGitmon "cat-file" $ lookupTree (commitTree commit)
          blobForPathInTree path tree = do
            entry <- reportGitmon "ls-tree" $ treeEntry tree (toS path)
            case entry of
              Just (BlobEntry entryOid entryKind) -> do
                blob <- reportGitmon "cat-file" $ lookupBlob entryOid
                contents <- blobToByteString blob
                transcoded <- liftIO $ transcode contents
                let oid = renderObjOid $ blobOid blob
                pure $! SourceBlob transcoded (toS oid) path (Just (toSourceKind entryKind))
              _ -> pure $! emptySourceBlob path
          pathsForTree tree = do
            blobEntries <- reportGitmon "ls-tree" $ treeBlobEntries tree
            return $! fmap (\ (p, _, _) -> toS p) blobEntries

          toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
          toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
          toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode

  Parse language blob -> parserForLanguage language blob >>= yield

  Diff term1 term2 ->
    yield (stripDiff (diffTerms (decorate term1) (decorate term2)))
    where decorate = defaultFeatureVectorDecorator getLabel
          getLabel :: TermF (Syntax Text) (Record DefaultFields) a -> (Category, Maybe Text)
          getLabel (h :< t) = (Info.category h, case t of
            Leaf s -> Just s
            _ -> Nothing)

  RenderDiff renderer blob1 blob2 diff ->
    yield (runDiffRenderer' renderer (both blob1 blob2) diff)
