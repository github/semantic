{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module DiffCommand where

import Data.Aeson hiding (json)
import Data.Functor.Both as Both
import Data.List ((\\))
import Data.String
import GHC.Conc (numCapabilities)
import Prologue hiding (fst, snd, null)
import qualified Control.Concurrent.Async.Pool as Async
import System.FilePath.Posix (hasExtension)
import System.Timeout (timeout)
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
import Info
import Interpreter
import ParseCommand (parserForFilepath)
import Parser
import Patch
import Renderer
import Renderer.JSON
import Renderer.Patch
import Renderer.SExpression
import Renderer.Split
import Renderer.Summary
import Renderer.TOC
import SemanticDiff.IO
import Source
import Syntax
import Term

diff :: Arguments -> IO Text
diff args@Arguments{..} = case diffMode of
    PathDiff paths -> diffPaths args paths
    CommitDiff -> diffCommits args

-- | Compare changes between two commits.
diffCommits :: Arguments -> IO Text
diffCommits args@Arguments{..} = do
  ts <- fetchTerms args
  pure $ maybe mempty concatOutputs ts
  where fetchTerms args = if developmentMode
                            then Just <$> fetchDiffs args
                            else timeout timeoutInMicroseconds (fetchDiffs args)

-- | Compare two paths on the filesystem (compariable to git diff --no-index).
diffPaths :: Arguments -> Both FilePath -> IO Text
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

  Async.withTaskGroup numCapabilities $ \p -> Async.mapTasks p (fetchDiff args <$> paths)

fetchDiff :: Arguments -> FilePath -> IO Output
fetchDiff args@Arguments{..} filepath = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)
  lift $ runReaderT (fetchDiff' args filepath) repo

fetchDiff' :: Arguments -> FilePath -> ReaderT LgRepo IO Output
fetchDiff' args@Arguments{..} filepath = do
  sourcesAndOids <- sequence $ traverse (getSourceBlob filepath) <$> shaRange

  let sources = fromMaybe (emptySourceBlob filepath) <$> sourcesAndOids
  let sourceBlobs = idOrEmptySourceBlob <$> sources
  let textDiff' = textDiff (parserForFilepath filepath) args sourceBlobs

  text <- fetchText textDiff'
  truncatedPatch <- liftIO $ truncatedDiff args sourceBlobs
  pure $ fromMaybe truncatedPatch text
  where
    fetchText textDiff = if developmentMode
                          then liftIO $ Just <$> textDiff
                          else liftIO $ timeout timeoutInMicroseconds textDiff

pathsToDiff :: Arguments -> Both String -> IO [FilePath]
pathsToDiff Arguments{..} shas = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)
  lift $ runReaderT (pathsToDiff' shas) repo

-- | Returns a list of relative file paths that have changed between the given commit shas.
pathsToDiff' :: Both String -> ReaderT LgRepo IO [FilePath]
pathsToDiff' shas = do
  entries <- blobEntriesToDiff shas
  pure $ (\(p, _, _) -> toS p) <$> entries

-- | Returns a list of blob entries that have changed between the given commits shas.
blobEntriesToDiff :: Both String -> ReaderT LgRepo IO [(TreeFilePath, Git.BlobOid LgRepo, BlobKind)]
blobEntriesToDiff shas = do
  a <- blobEntries (fst shas)
  b <- blobEntries (snd shas)
  pure $ (a \\ b) <> (b \\ a)
  where blobEntries sha = treeForCommitSha sha >>= treeBlobEntries

-- | Returns a Git.Tree for a commit sha
treeForCommitSha :: String -> ReaderT LgRepo IO (Git.Tree LgRepo)
treeForCommitSha sha = do
  object <- parseObjOid (toS sha)
  commit <- lookupCommit object
  lookupTree (commitTree commit)

-- | Returns a SourceBlob given a relative file path, and the sha to look up.
getSourceBlob :: FilePath -> String -> ReaderT LgRepo IO SourceBlob
getSourceBlob path sha = do
  tree <- treeForCommitSha sha
  entry <- treeEntry tree (toS path)
  (bytestring, oid, mode) <- case entry of
    Nothing -> pure (mempty, mempty, Nothing)
    Just (BlobEntry entryOid entryKind) -> do
      blob <- lookupBlob entryOid
      s <- blobToByteString blob
      let oid = renderObjOid $ blobOid blob
      pure (s, oid, Just entryKind)
  s <- liftIO $ transcode bytestring
  pure $ SourceBlob s (toS oid) path (toSourceKind <$> mode)
  where
    toSourceKind :: Git.BlobKind -> SourceKind
    toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
    toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
    toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode

-- | Given a parser and renderer, diff two sources and return the rendered
-- | result.
-- | Returns the rendered result strictly, so it's always fully evaluated
-- | with respect to other IO actions.
diffFiles :: HasField fields Category
          => Parser (Syntax Text) (Record fields)
          -> Renderer (Record fields)
          -> Both SourceBlob
          -> IO Output
diffFiles parse render sourceBlobs = do
  terms <- traverse (fmap (defaultFeatureVectorDecorator getLabel) . parse) sourceBlobs
  pure $! render sourceBlobs (stripDiff (diffTerms' terms))

  where
    diffTerms' terms = case runBothWith areNullOids sourceBlobs of
        (True, False) -> pure $ Insert (snd terms)
        (False, True) -> pure $ Delete (fst terms)
        (_, _) ->
          runBothWith diffTerms terms
    areNullOids a b = (hasNullOid a, hasNullOid b)
    hasNullOid blob = oid blob == nullOid || null (source blob)

getLabel :: HasField fields Category => CofreeF (Syntax leaf) (Record fields) b -> (Category, Maybe leaf)
getLabel (h :< t) = (category h, case t of
  Leaf s -> Just s
  _ -> Nothing)

-- | Determine whether two terms are comparable based on the equality of their categories.
compareCategoryEq :: Functor f => HasField fields Category => Term f (Record fields) -> Term f (Record fields) -> Bool
compareCategoryEq = (==) `on` category . extract

-- | Returns a rendered diff given a parser, diff arguments and two source blobs.
textDiff :: (ToJSON (Record fields), DefaultFields fields) => Parser (Syntax Text) (Record fields) -> Arguments -> Both SourceBlob -> IO Output
textDiff parser arguments = diffFiles parser $ case format arguments of
  Split -> split
  Patch -> patch
  SExpression -> sExpression TreeOnly
  JSON -> json
  Summary -> summary
  TOC -> toc

-- | Returns a truncated diff given diff arguments and two source blobs.
truncatedDiff :: Arguments -> Both SourceBlob -> IO Output
truncatedDiff Arguments{..} sources = pure $ case format of
  Split -> SplitOutput mempty
  Patch -> PatchOutput (truncatePatch sources)
  SExpression -> SExpressionOutput mempty
  JSON -> JSONOutput mempty
  Summary -> SummaryOutput mempty
  TOC -> TOCOutput mempty

-- | Prints a rendered diff to stdio or a filepath given a parser, diff arguments and two source blobs.
printDiff :: (ToJSON (Record fields), DefaultFields fields) => Parser (Syntax Text) (Record fields) -> Arguments -> Both SourceBlob -> IO Text
printDiff parser arguments sources = do
  rendered <- textDiff parser arguments sources
  pure $ case rendered of
    SplitOutput text -> text
    PatchOutput text -> text
    SExpressionOutput text -> text
    JSONOutput series -> encodingToText (toJSON series)
    SummaryOutput summaries -> encodingToText (toJSON summaries)
    TOCOutput summaries -> encodingToText (toJSON summaries)
  where
    encodingToText = toS . encode
