{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module Command.Diff where

import Data.Aeson hiding (json)
import Data.Functor.Both as Both
import Data.List ((\\))
import Data.String
import GHC.Conc (numCapabilities)
import Prologue hiding (fst, snd, null)
import qualified Control.Concurrent.Async.Pool as Async
import System.FilePath.Posix (hasExtension)
import Git.Blob
import Git.Libgit2
import Git.Libgit2.Backend
import Git.Repository
import Git.Types
import qualified Git
import Arguments
import Category
import Data.RandomWalkSimilarity
import Data.Record
import GitmonClient
import Info
import Diff
import Interpreter
import Command.Parse (parserForFilepath)
import Parser
import Patch
import Renderer
import Renderer.JSON
import Renderer.Patch
import Renderer.SExpression
import Renderer.Split
import Renderer.Summary
import Renderer.TOC
import Source
import Syntax
import Debug.Trace

diff :: Arguments -> IO ByteString
diff args@Arguments{..} = case diffMode of
    PathDiff paths -> diffPaths args paths
    CommitDiff -> diffCommits args

-- | Compare changes between two commits.
diffCommits :: Arguments -> IO ByteString
diffCommits args@Arguments{..} = do
  outputs <- fetchDiffs args
  pure $! concatOutputs outputs

-- | Compare two paths on the filesystem (compariable to git diff --no-index).
diffPaths :: Arguments -> Both FilePath -> IO ByteString
diffPaths args@Arguments{..} paths = do
  sources <- traverse readAndTranscodeFile paths
  let sourceBlobs = SourceBlob <$> sources <*> pure mempty <*> paths <*> pure (Just defaultPlainBlob)
  printDiff (parserForFilepath path) args sourceBlobs
  where
    path = fromMaybe (panic "none of the paths have file extensions") $ find hasExtension paths

fetchDiffs :: Arguments -> IO [Output]
fetchDiffs args@Arguments{..} = do
  paths <- case(filePaths, shaRange) of
    ([], Join (Just a, Just b)) -> pathsToDiff args (both a b)
    (ps, _) -> pure ps

  diffs <- Async.withTaskGroup numCapabilities . flip Async.mapTasks $
    fetchDiff args <$> paths
  pure $ uncurry (renderDiff args) <$> diffs

fetchDiff :: Arguments -> FilePath -> IO (Both SourceBlob, SyntaxDiff Text DefaultFields)
fetchDiff args@Arguments{..} filepath = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)
  go args filepath
  where
    go :: Arguments -> FilePath -> ReaderT LgRepo IO (Both SourceBlob, SyntaxDiff Text DefaultFields)
    go Arguments{..} filepath = do
      liftIO $ traceEventIO ("START fetchDiff: " <> filepath)
      sourcesAndOids <- sequence $ traverse (getSourceBlob filepath) <$> shaRange

      let sources = fromMaybe (emptySourceBlob filepath) <$> sourcesAndOids
      let sourceBlobs = idOrEmptySourceBlob <$> sources

      diff <- liftIO $ diffFiles (parserForFilepath filepath) sourceBlobs
      pure $! traceEvent ("END fetchDiff: " <> filepath) (sourceBlobs, diff)

-- | Returns a list of relative file paths that have changed between the given commit shas.
pathsToDiff :: Arguments -> Both String -> IO [FilePath]
pathsToDiff Arguments{..} shas = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)
  go shas
  where
    go :: Both String -> ReaderT LgRepo IO [FilePath]
    go shas = do
      entries <- blobEntriesToDiff shas
      pure $ (\(p, _, _) -> toS p) <$> entries

-- | Returns a list of blob entries that have changed between the given commits shas.
blobEntriesToDiff :: Both String -> ReaderT LgRepo IO [(TreeFilePath, Git.BlobOid LgRepo, BlobKind)]
blobEntriesToDiff shas = do
  a <- blobEntries (fst shas)
  b <- blobEntries (snd shas)
  pure $ (a \\ b) <> (b \\ a)
  where blobEntries sha = treeForCommitSha sha >>= treeBlobEntries'
        treeBlobEntries' tree = reportGitmon "ls-tree" $ treeBlobEntries tree

-- | Returns a Git.Tree for a commit sha
treeForCommitSha :: String -> ReaderT LgRepo IO (Git.Tree LgRepo)
treeForCommitSha sha = do
  object <- parseObjOid (toS sha)
  commit <- reportGitmon "cat-file" $ lookupCommit object
  reportGitmon "cat-file" $ lookupTree (commitTree commit)

-- | Returns a SourceBlob given a relative file path, and the sha to look up.
getSourceBlob :: FilePath -> String -> ReaderT LgRepo IO Source.SourceBlob
getSourceBlob path sha = do
  tree <- treeForCommitSha sha
  entry <- reportGitmon "ls-tree" $ treeEntry tree (toS path)
  (bytestring, oid, mode) <- case entry of
    Nothing -> pure (mempty, mempty, Nothing)
    Just (BlobEntry entryOid entryKind) -> do
      blob <- reportGitmon "cat-file" $ lookupBlob entryOid
      s <- blobToByteString blob
      let oid = renderObjOid $ blobOid blob
      pure (s, oid, Just entryKind)
  s <- liftIO $ transcode bytestring
  pure $ Source.SourceBlob s (toS oid) path (toSourceKind <$> mode)
  where
    toSourceKind :: Git.BlobKind -> SourceKind
    toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
    toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
    toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode

-- | Given a parser, diff two sources and return a SyntaxDiff.
-- | Returns the rendered result strictly, so it's always fully evaluated with respect to other IO actions.
diffFiles :: (HasField fields Category, NFData (Record fields))
          => Parser (Syntax Text) (Record fields)
          -> Both SourceBlob
          -> IO (SyntaxDiff Text fields)
diffFiles parse sourceBlobs = do
  traceEventIO $ "diffFiles@SEMANTIC-DIFF START parse terms: " <> paths
  terms <- Async.withTaskGroup numCapabilities . flip Async.mapTasks $
    (fmap (defaultFeatureVectorDecorator getLabel) . parse) <$> sourceBlobs
  traceEventIO $ "diffFiles@SEMANTIC-DIFF END parse terms: " <> paths
  traceEventIO $ "diffFiles@SEMANTIC-DIFF START diff terms: " <> paths
  traceEvent ("diffFiles@SEMANTIC-DIFF END diff terms: " <> paths) pure $!! stripDiff (diffTerms' terms)
  where
    diffTerms' terms = case runBothWith areNullOids sourceBlobs of
        (True, False) -> pure $ Insert (snd terms)
        (False, True) -> pure $ Delete (fst terms)
        (_, _) ->
          runBothWith diffTerms terms
    areNullOids a b = (hasNullOid a, hasNullOid b)
    hasNullOid blob = oid blob == nullOid || null (source blob)
    -- For trace events
    paths = runBothWith (\a b -> fileAtSha a <> " -> " <> fileAtSha b) sourceBlobs
    fileAtSha x = path x <> "@" <> toS (oid x)

    getLabel :: HasField fields Category => CofreeF (Syntax leaf) (Record fields) b -> (Category, Maybe leaf)
    getLabel (h :< t) = (category h, case t of
      Leaf s -> Just s
      _ -> Nothing)

-- | Returns a rendered diff given arguments and two source blobs.
renderDiff :: (ToJSON (Record fields), NFData (Record fields), HasDefaultFields fields) => Arguments -> Both SourceBlob -> SyntaxDiff Text fields -> Output
renderDiff args = case format args of
  Split -> split
  Patch -> patch
  SExpression -> (SExpressionOutput .) . sExpression TreeOnly
  JSON -> json
  Summary -> summary
  TOC -> toc

-- | Prints a rendered diff to stdio or a filepath given a parser, arguments and two source blobs.
printDiff :: (ToJSON (Record fields), NFData (Record fields), HasDefaultFields fields) => Parser (Syntax Text) (Record fields) -> Arguments -> Both SourceBlob -> IO ByteString
printDiff parser args sources = do
  diff <- diffFiles parser sources
  pure $! concatOutputs [renderDiff args sources diff]
