{-# LANGUAGE DataKinds, GADTs #-}
module Command
( Command
-- Constructors
, readFile
, readFilesAtSHAs
, parse
, parseBlob
, diff
, renderDiff
, renderDiffOutput
-- Evaluation
, runCommand
) where

import Command.Parse
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
import Git.Libgit2.Backend
import Git.Repository
import Git.Types
import GitmonClient
import Language
import Prologue hiding (readFile)
import Renderer
import Source
import Syntax
import System.FilePath
import Term


-- | High-level commands encapsulating the work done to perform a diff or parse operation.
type Command = Freer CommandF


-- Constructors

-- | Read a regular file into a SourceBlob.
readFile :: FilePath -> Command SourceBlob
readFile path = ReadFile path `Then` return

-- | Read a list of files at the states corresponding to the given shas.
readFilesAtSHAs
  :: FilePath -- ^ GIT_DIR
  -> [FilePath] -- ^ GIT_ALTERNATE_OBJECT_DIRECTORIES
  -> [FilePath] -- ^ Specific paths to diff. If empty, diff all changed paths.
  -> String -- ^ The commit sha for the before state.
  -> String -- ^ The commit sha for the after state.
  -> Command [(SourceBlob, SourceBlob)] -- ^ A command producing a list of pairs of blobs for the specified files (or all files if none were specified).
readFilesAtSHAs gitDir alternateObjectDirs paths sha1 sha2 = ReadFilesAtSHAs gitDir alternateObjectDirs paths sha1 sha2 `Then` return

-- | Parse a blob in a given language.
parse :: Maybe Language -> SourceBlob -> Command (Term (Syntax Text) (Record DefaultFields))
parse language blob = Parse language blob `Then` return

-- | Parse a blob in the language selected for its file extension.
parseBlob :: SourceBlob -> Command (Term (Syntax Text) (Record DefaultFields))
parseBlob blob = parse (languageForType (takeExtension (path blob))) blob

-- | Diff two terms.
diff :: HasField fields Category => Term (Syntax Text) (Record fields) -> Term (Syntax Text) (Record fields) -> Command (Diff (Syntax Text) (Record fields))
diff term1 term2 = Diff term1 term2 `Then` return

-- | Render a diff using the specified renderer.
renderDiff :: DiffRenderer fields output -> SourceBlob -> SourceBlob -> Diff (Syntax Text) (Record fields) -> Command output
renderDiff renderer blob1 blob2 diff = RenderDiff renderer blob1 blob2 diff `Then` return

-- | Render a diff using the specified renderer, wrapping the result up in Output.
renderDiffOutput :: DiffRenderer fields output -> SourceBlob -> SourceBlob -> Diff (Syntax Text) (Record fields) -> Command Output
renderDiffOutput renderer blob1 blob2 diff = fmap output (renderDiff renderer blob1 blob2 diff)
  where output = case renderer of
          SplitRenderer -> SplitOutput
          PatchRenderer -> PatchOutput
          JSONDiffRenderer -> JSONOutput
          SummaryRenderer -> SummaryOutput . unSummaries
          SExpressionDiffRenderer _ -> SExpressionOutput
          ToCRenderer -> TOCOutput . unSummaries


-- Evaluation

-- | Run the passed command and return its results in IO.
runCommand :: Command a -> IO a
runCommand = iterFreerA $ \ command yield -> case command of
  ReadFile path -> runReadFile path >>= yield
  ReadFilesAtSHAs gitDir alternateObjectDirs paths sha1 sha2 -> runReadFilesAtSHAs gitDir alternateObjectDirs paths sha1 sha2 >>= yield
  Parse language blob -> runParse language blob >>= yield
  Diff term1 term2 -> yield (runDiff term1 term2)
  RenderDiff renderer blob1 blob2 diff -> yield (runRenderDiff renderer blob1 blob2 diff)


-- Implementation details

data CommandF f where
  ReadFile :: FilePath -> CommandF SourceBlob
  ReadFilesAtSHAs :: FilePath -> [FilePath] -> [FilePath] -> String -> String -> CommandF [(SourceBlob, SourceBlob)]

  Parse :: Maybe Language -> SourceBlob -> CommandF (Term (Syntax Text) (Record DefaultFields))

  Diff :: HasField fields Category => Term (Syntax Text) (Record fields) -> Term (Syntax Text) (Record fields) -> CommandF (Diff (Syntax Text) (Record fields))

  RenderDiff :: DiffRenderer fields output -> SourceBlob -> SourceBlob -> Diff (Syntax Text) (Record fields) -> CommandF output

  -- TODO: parallelize diffs of a list of paths + git shas?


runReadFile :: FilePath -> IO SourceBlob
runReadFile path = do
  source <- readAndTranscodeFile path
  return (sourceBlob source path)

runReadFilesAtSHAs :: FilePath -> [FilePath] -> [FilePath] -> String -> String -> IO [(SourceBlob, SourceBlob)]
runReadFilesAtSHAs gitDir alternateObjectDirs paths sha1 sha2 = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)

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
  return blobs
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

runParse :: Maybe Language -> SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
runParse = maybe lineByLineParser parserForLanguage

runDiff :: HasField fields Category => Term (Syntax Text) (Record fields) -> Term (Syntax Text) (Record fields) -> Diff (Syntax Text) (Record fields)
runDiff term1 term2 = stripDiff (diffTerms (decorate term1) (decorate term2))
  where decorate = defaultFeatureVectorDecorator getLabel
        getLabel :: HasField fields Category => TermF (Syntax Text) (Record fields) a -> (Category, Maybe Text)
        getLabel (h :< t) = (Info.category h, case t of
          Leaf s -> Just s
          _ -> Nothing)

runRenderDiff :: DiffRenderer fields output -> SourceBlob -> SourceBlob -> Diff (Syntax Text) (Record fields) -> output
runRenderDiff renderer = (runDiffRenderer' renderer .) . both
