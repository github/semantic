{-# LANGUAGE DataKinds, GADTs, TypeOperators, BangPatterns #-}
module Semantic.Task
( Task
, Level(..)
, RAlgebra
, Differ
, readBlobs
, readBlobPairs
, writeToOutput
, writeLog
, time
, parse
, decorate
, diff
, render
, distribute
, distributeFor
, distributeFoldMap
, defaultOptions
, configureOptionsForHandle
, terminalFormatter
, logfmtFormatter
, runTask
, runTaskWithOptions
) where

import Control.Concurrent.STM.TMQueue
import Control.Exception
import Control.Monad (join)
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Blob
import qualified Data.ByteString as B
import Data.Foldable (fold, for_)
import Data.Functor.Both as Both hiding (snd)
import Data.Functor.Foldable (cata)
import Data.Record
import Data.Semigroup ((<>))
import Data.Source (totalRange, totalSpan)
import qualified Data.Syntax as Syntax
import Data.Syntax.Algebra (RAlgebra, decoratorWithAlgebra)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import qualified Data.Time.LocalTime as LocalTime
import Data.Union
import Diff
import Info
import qualified Files
import GHC.Conc (atomically)
import Language
import Language.Markdown
import Parser
import System.Exit (die)
import System.IO (Handle, hPutStr, stderr)
import Term
import TreeSitter
import Semantic.Log

data TaskF output where
  ReadBlobs :: Either Handle [(FilePath, Maybe Language)] -> TaskF [Blob]
  ReadBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> TaskF [Both Blob]
  WriteToOutput :: Either Handle FilePath -> B.ByteString -> TaskF ()
  WriteLog :: Level -> String -> [(String, String)] -> TaskF ()
  Time :: String -> [(String, String)] -> Task output -> TaskF output
  Parse :: Parser term -> Blob -> TaskF term
  Decorate :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> TaskF (Term f (Record (field ': fields)))
  Diff :: Differ f a -> Both (Term f a) -> TaskF (Diff f a)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)
  LiftIO :: IO a -> TaskF a

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type Task = Freer TaskF

-- | A function to compute the 'Diff' for a pair of 'Term's with arbitrary syntax functor & annotation types.
type Differ f a = Both (Term f a) -> Diff f a

-- | A function to render terms or diffs.
type Renderer i o = i -> o

-- | A 'Task' which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Either Handle [(FilePath, Maybe Language)] -> Task [Blob]
readBlobs from = ReadBlobs from `Then` return

-- | A 'Task' which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> Task [Both Blob]
readBlobPairs from = ReadBlobPairs from `Then` return

-- | A 'Task' which writes a 'B.ByteString' to a 'Handle' or a 'FilePath'.
writeToOutput :: Either Handle FilePath -> B.ByteString -> Task ()
writeToOutput path contents = WriteToOutput path contents `Then` return


-- | A 'Task' which logs a message at a specific log level to stderr.
writeLog :: Level -> String -> [(String, String)] -> Task ()
writeLog level message pairs = WriteLog level message pairs `Then` return

-- | A 'Task' which measures and logs the timing of another 'Task'.
time :: String -> [(String, String)] -> Task output -> Task output
time message pairs task = Time message pairs task `Then` return

-- | A 'Task' which parses a 'Blob' with the given 'Parser'.
parse :: Parser term -> Blob -> Task term
parse parser blob = Parse parser blob `Then` return

-- | A 'Task' which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> Task (Term f (Record (field ': fields)))
decorate algebra term = Decorate algebra term `Then` return

-- | A 'Task' which diffs a pair of terms using the supplied 'Differ' function.
diff :: Differ f a -> Both (Term f a) -> Task (Diff f a)
diff differ terms = Diff differ terms `Then` return

-- | A 'Task' which renders some input using the supplied 'Renderer' function.
render :: Renderer input output -> input -> Task output
render renderer input = Render renderer input `Then` return

-- | Distribute a 'Traversable' container of 'Task's over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: Traversable t => t (Task output) -> Task (t output)
distribute tasks = Distribute tasks `Then` return

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: Traversable t => t a -> (a -> Task output) -> Task (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Traversable t, Monoid output) => (a -> Task output) -> t a -> Task output
distributeFoldMap toTask inputs = fmap fold (distribute (fmap toTask inputs))

-- | Execute a 'Task' with the 'defaultOptions', yielding its result value in 'IO'.
--
-- > runTask = runTaskWithOptions defaultOptions
runTask :: Task a -> IO a
runTask = runTaskWithOptions defaultOptions

-- | Execute a 'Task' with the passed 'Options', yielding its result value in 'IO'.
runTaskWithOptions :: Options -> Task a -> IO a
runTaskWithOptions options task = do
  options <- configureOptionsForHandle stderr options
  logQueue <- newTMQueueIO
  logging <- Async.async (logSink options logQueue)

  result <- run options logQueue task
  atomically (closeTMQueue logQueue)
  Async.wait logging
  either die pure result
  where logSink options@Options{..} queue = do
          message <- atomically (readTMQueue queue)
          case message of
            Just message -> do
              hPutStr stderr (optionsFormatter options message)
              logSink options queue
            _ -> pure ()
        run :: Options -> TMQueue Message -> Task a -> IO (Either String a)
        run options logQueue = go
          where go :: Task a -> IO (Either String a)
                go = iterFreerA (\ task yield -> case task of
                  ReadBlobs source -> (either Files.readBlobsFromHandle (traverse (uncurry Files.readFile)) source >>= yield) `catchError` (pure . Left . displayException)
                  ReadBlobPairs source -> (either Files.readBlobPairsFromHandle (traverse (traverse (uncurry Files.readFile))) source >>= yield) `catchError` (pure . Left . displayException)
                  WriteToOutput destination contents -> either B.hPutStr B.writeFile destination contents >>= yield
                  WriteLog level message pairs -> queueLogMessage level message pairs >>= yield
                  Time message pairs task -> do
                    start <- Time.getCurrentTime
                    !res <- go task
                    end <- Time.getCurrentTime
                    queueLogMessage Info message (pairs <> [("duration", show (Time.diffUTCTime end start))])
                    either (pure . Left) yield res
                  Parse parser blob -> go (runParser options blob parser) >>= either (pure . Left) yield . join
                  Decorate algebra term -> pure (decoratorWithAlgebra algebra term) >>= yield
                  Diff differ terms -> pure (differ terms) >>= yield
                  Render renderer input -> pure (renderer input) >>= yield
                  Distribute tasks -> Async.mapConcurrently go tasks >>= either (pure . Left) yield . sequenceA . withStrategy (parTraversable (parTraversable rseq))
                  LiftIO action -> action >>= yield ) . fmap Right
                queueLogMessage level message pairs
                  | Just logLevel <- optionsLevel options, level <= logLevel = Time.getCurrentTime >>= LocalTime.utcToLocalZonedTime >>= atomically . writeTMQueue logQueue . Message level message pairs
                  | otherwise = pure ()


runParser :: Options -> Blob -> Parser term -> Task (Either String term)
runParser Options{..} blob@Blob{..} = go
  where go :: Parser term -> Task (Either String term)
        go parser = case parser of
          ASTParser language -> do
            logTiming "ts ast parse" $
              liftIO $ (Right <$> parseToAST language blob) `catchError` (pure . Left. displayException)
          AssignmentParser parser by assignment -> do
            res <- go parser
            case res of
              Left err -> writeLog Error "failed parsing" blobFields >> pure (Left err)
              Right ast -> logTiming "assign" $ case Assignment.assignBy by blobSource assignment ast of
                Left Assignment.Error{..} -> do
                  writeLog Error (Assignment.formatError optionsPrintSource (optionsIsTerminal && optionsEnableColour) blob errorSpan (show <$> errorExpected) (show <$> errorActual)) blobFields
                  pure $ Right (Syntax.makeTerm (totalRange blobSource :. totalSpan blobSource :. Nil) (Syntax.Error (show <$> errorExpected) (show <$> errorActual) []))
                Right term -> do
                  for_ (errors term) $ \ (errorSpan, errorExpected, errorActual) ->
                    writeLog Warning (Assignment.formatError optionsPrintSource optionsEnableColour blob errorSpan errorExpected errorActual) blobFields
                  pure $ Right term
          TreeSitterParser tslanguage -> logTiming "ts parse" $ liftIO (Right <$> treeSitterParser tslanguage blob)
          MarkdownParser -> logTiming "cmark parse" $ pure (Right (cmarkParser blobSource))
          LineByLineParser -> logTiming "line-by-line parse" $ pure (Right (lineByLineParser blobSource))
        blobFields = [ ("path", blobPath), ("language", maybe "" show blobLanguage) ]
        errors :: (Syntax.Error :< fs, Foldable (Union fs), Functor (Union fs)) => Term (Union fs) (Record Assignment.Location) -> [(Span, [String], Maybe String)]
        errors = cata $ \ (a :< syntax) -> case syntax of
          _ | Just (Syntax.Error expected actual _) <- prj syntax -> [(sourceSpan a, expected, actual)]
          _ -> fold syntax
        logTiming :: String -> Task a -> Task a
        logTiming msg = time msg blobFields

instance MonadIO Task where
  liftIO action = LiftIO action `Then` return
