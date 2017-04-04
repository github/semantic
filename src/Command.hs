{-# LANGUAGE DataKinds, GADTs #-}
module Command
( Command
-- Constructors
, readFile
, readFilesAtSHAs
, parse
, parseBlob
, diff
, maybeDiff
, renderDiffs
, concurrently
-- Evaluation
, runCommand
) where

import Command.Parse
import qualified Control.Concurrent.Async.Pool as Async
import Control.Exception (catch)
import Control.Monad.Free.Freer
import Control.Parallel.Strategies
import qualified Data.ByteString as B
import Data.Functor.Both
import Data.List ((\\))
import Data.RandomWalkSimilarity
import Data.Record
import Data.String
import Debug.Trace (traceEventIO)
import Diff
import Info
import Interpreter
import GHC.Conc (numCapabilities)
import qualified Git
import Git.Blob
import Git.Libgit2
import Git.Libgit2.Backend
import Git.Repository
import Git.Types
import GitmonClient
import Language
import Patch
import Prologue hiding (concurrently, Concurrently, readFile)
import Renderer
import Source
import Syntax
import System.FilePath
import Term


-- | High-level commands encapsulating the work done to perform a diff or parse operation.
type Command = Freer CommandF


-- Constructors

-- | Read a regular file into a SourceBlob.
readFile :: FilePath -> Command (Maybe SourceBlob)
readFile path = ReadFile path `Then` return

-- | Read a list of files at the states corresponding to the given shas.
readFilesAtSHAs
  :: FilePath -- ^ GIT_DIR
  -> [FilePath] -- ^ GIT_ALTERNATE_OBJECT_DIRECTORIES
  -> [FilePath] -- ^ Specific paths to diff. If empty, diff all changed paths.
  -> Both String -- ^ The commit shas for the before & after states.
  -> Command [(FilePath, Both (Maybe SourceBlob))] -- ^ A command producing a list of pairs of blobs for the specified files (or all files if none were specified).
readFilesAtSHAs gitDir alternateObjectDirs paths shas = ReadFilesAtSHAs gitDir alternateObjectDirs paths shas `Then` return

-- | Parse a blob in a given language.
parse :: Maybe Language -> SourceBlob -> Command (Term (Syntax Text) (Record DefaultFields))
parse language blob = Parse language blob `Then` return

-- | Parse a blob in the language selected for its file extension.
parseBlob :: SourceBlob -> Command (Term (Syntax Text) (Record DefaultFields))
parseBlob blob = parse (languageForType (takeExtension (path blob))) blob

-- | Diff two terms.
diff :: HasField fields Category => Both (Term (Syntax Text) (Record fields)) -> Command (Diff (Syntax Text) (Record fields))
diff terms = Diff (terms `using` parTraversable rpar) `Then` return

-- | Diff two terms, producing an insertion/deletion when one is missing and Nothing when both are missing.
maybeDiff :: HasField fields Category => Both (Maybe (Term (Syntax Text) (Record fields))) -> Command (Maybe (Diff (Syntax Text) (Record fields)))
maybeDiff terms = case runJoin terms of
  (Just term1, Nothing) -> return (Just (pure (Delete term1)))
  (Nothing, Just term2) -> return (Just (pure (Insert term2)))
  (Just term1, Just term2) -> Just <$> diff (both term1 term2)
  (Nothing, Nothing) -> return Nothing

-- | Render a diff using the specified renderer.
renderDiffs :: (NFData (Record fields), Monoid output) => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> Command output
renderDiffs renderer diffs = RenderDiffs renderer (diffs `using` parTraversable (parTuple2 r0 rdeepseq)) `Then` return

-- | Run a function over each element of a Traversable concurrently.
concurrently :: Traversable t => t a -> (a -> Command b) -> Command (t b)
concurrently ts f = Concurrently ts f `Then` return


-- Evaluation

-- | Run the passed command and return its results in IO.
runCommand :: Command a -> IO a
runCommand = iterFreerA $ \ command yield -> case command of
  ReadFile path -> runReadFile path >>= yield
  ReadFilesAtSHAs gitDir alternateObjectDirs paths shas -> runReadFilesAtSHAs gitDir alternateObjectDirs paths shas >>= yield
  Parse language blob -> runParse language blob >>= yield
  Diff terms -> yield (runDiff terms)
  RenderDiffs renderer diffs -> yield (runRenderDiffs renderer diffs)
  Concurrently ts f -> do
    results <- Async.withTaskGroup numCapabilities $ \ group -> Async.runTask group $ traverse (Async.task . runCommand . f) ts
    yield results


-- Implementation details

data CommandF f where
  ReadFile :: FilePath -> CommandF (Maybe SourceBlob)
  ReadFilesAtSHAs :: FilePath -> [FilePath] -> [FilePath] -> Both String -> CommandF [(FilePath, Both (Maybe SourceBlob))]

  Parse :: Maybe Language -> SourceBlob -> CommandF (Term (Syntax Text) (Record DefaultFields))

  Diff :: HasField fields Category => Both (Term (Syntax Text) (Record fields)) -> CommandF (Diff (Syntax Text) (Record fields))

  RenderDiffs :: Monoid output => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> CommandF output

  Concurrently :: Traversable t => t a -> (a -> Command b) -> CommandF (t b)


runReadFile :: FilePath -> IO (Maybe SourceBlob)
runReadFile path = do
  raw <- (Just <$> B.readFile path) `catch` (const (return Nothing) :: IOException -> IO (Maybe ByteString))
  source <- traverse transcode raw
  return (flip sourceBlob path <$> source)

runReadFilesAtSHAs :: FilePath -> [FilePath] -> [FilePath] -> Both String -> IO [(FilePath, Both (Maybe SourceBlob))]
runReadFilesAtSHAs gitDir alternateObjectDirs paths shas = withRepository lgFactory gitDir $ do
  repo <- getRepository
  for_ alternateObjectDirs (liftIO . odbBackendAddPath repo . toS)

  liftIO $ traceEventIO ("START readFilesAtSHAs: " <> show paths)

  trees <- traverse treeForSha shas

  paths <- case paths of
    (_ : _) -> pure paths
    [] -> do
      paths <- traverse pathsForTree trees
      pure $! runBothWith (\\) paths <> runBothWith (flip (\\)) paths

  blobs <- for paths $ \ path -> (,) path <$> traverse (blobForPathInTree path) trees

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
              pure (Just (SourceBlob transcoded (toS oid) path (Just (toSourceKind entryKind))))
            _ -> pure Nothing
        pathsForTree tree = do
          blobEntries <- reportGitmon "ls-tree" $ treeBlobEntries tree
          return $! fmap (\ (p, _, _) -> toS p) blobEntries

        toSourceKind (Git.PlainBlob mode) = Source.PlainBlob mode
        toSourceKind (Git.ExecutableBlob mode) = Source.ExecutableBlob mode
        toSourceKind (Git.SymlinkBlob mode) = Source.SymlinkBlob mode

runParse :: Maybe Language -> SourceBlob -> IO (Term (Syntax Text) (Record DefaultFields))
runParse = maybe lineByLineParser parserForLanguage

runDiff :: HasField fields Category => Both (Term (Syntax Text) (Record fields)) -> Diff (Syntax Text) (Record fields)
runDiff terms = stripDiff (runBothWith diffTerms (fmap decorate terms))
  where decorate = defaultFeatureVectorDecorator getLabel
        getLabel :: HasField fields Category => TermF (Syntax Text) (Record fields) a -> (Category, Maybe Text)
        getLabel (h :< t) = (Info.category h, case t of
          Leaf s -> Just s
          _ -> Nothing)

runRenderDiffs :: Monoid output => DiffRenderer fields output -> [(Both SourceBlob, Diff (Syntax Text) (Record fields))] -> output
runRenderDiffs = runDiffRenderer
