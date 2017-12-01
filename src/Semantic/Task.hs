{-# LANGUAGE DataKinds, GADTs, MultiParamTypeClasses, TypeOperators #-}
module Semantic.Task
( Task
, Level(..)
, RAlgebra
, Differ
, readBlobs
, readBlobPairs
, writeToOutput
, writeLog
, writeStat
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

import Analysis.Decorator (decoratorWithAlgebra)
import qualified Assigning.Assignment as Assignment
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Algebra (RAlgebra)
import Data.Blob
import Data.Bool
import qualified Data.ByteString as B
import Data.Diff
import qualified Data.Error as Error
import Data.Foldable (fold, for_)
import Data.Functor.Both as Both hiding (snd)
import Data.Functor.Foldable (cata)
import Data.Language
import Data.Record
import qualified Data.Syntax as Syntax
import Data.Term
import Data.Union
import Info hiding (Category(..))
import Parsing.Parser
import Parsing.CMark
import Parsing.TreeSitter
import System.Exit (die)
import System.IO (Handle, stderr)
import qualified Semantic.IO as IO
import Semantic.Log
import Semantic.Stat as Stat
import Semantic.Queue


data TaskF output where
  ReadBlobs :: Either Handle [(FilePath, Maybe Language)] -> TaskF [Blob]
  ReadBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> TaskF [Both Blob]
  WriteToOutput :: Either Handle FilePath -> B.ByteString -> TaskF ()
  WriteLog :: Level -> String -> [(String, String)] -> TaskF ()
  WriteStat :: Stat -> TaskF ()
  Time :: String -> [(String, String)] -> Task output -> TaskF output
  Parse :: Parser term -> Blob -> TaskF term
  Decorate :: Functor f => RAlgebra (Term f (Record fields)) field -> Term f (Record fields) -> TaskF (Term f (Record (field ': fields)))
  Diff :: Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> TaskF (Diff syntax ann1 ann2)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)

  -- | For MonadIO.
  LiftIO :: IO a -> TaskF a

  -- | For MonadError.
  Throw :: SomeException -> TaskF a
  Catch :: Task a -> (SomeException -> Task a) -> TaskF a

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type Task = Freer TaskF

-- | A function to compute the 'Diff' for a pair of 'Term's with arbitrary syntax functor & annotation types.
type Differ syntax ann1 ann2 = Term syntax ann1 -> Term syntax ann2 -> Diff syntax ann1 ann2

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

-- | A 'Task' which writes a stat.
writeStat :: Stat -> Task ()
writeStat stat = WriteStat stat `Then` return

-- | A 'Task' which measures and stats the timing of another 'Task'.
time :: String -> [(String, String)] -> Task output -> Task output
time statName tags task = Time statName tags task `Then` return

-- | A 'Task' which parses a 'Blob' with the given 'Parser'.
parse :: Parser term -> Blob -> Task term
parse parser blob = Parse parser blob `Then` return

-- | A 'Task' which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: Functor f => RAlgebra (Term f (Record fields)) field -> Term f (Record fields) -> Task (Term f (Record (field ': fields)))
decorate algebra term = Decorate algebra term `Then` return

-- | A 'Task' which diffs a pair of terms using the supplied 'Differ' function.
diff :: Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Task (Diff syntax ann1 ann2)
diff differ term1 term2 = Semantic.Task.Diff differ term1 term2 `Then` return

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
  statter <- defaultStatsClient >>= newQueue sendStat
  logger <- newQueue logMessage options

  result <- withTiming (queue statter) "run" [] $
    run options logger statter task

  closeQueue statter
  closeStatClient (asyncQueueExtra statter)
  closeQueue logger
  either (die . displayException) pure result
  where
    run :: Options
        -> AsyncQueue Message Options
        -> AsyncQueue Stat StatsClient
        -> Task a
        -> IO (Either SomeException a)
    run options logger statter = go
      where
        go :: Task a -> IO (Either SomeException a)
        go = iterFreerA (\ yield task -> case task of
          ReadBlobs (Left handle) -> (IO.readBlobsFromHandle handle >>= yield) `catchError` (pure . Left . toException)
          ReadBlobs (Right paths@[(path, Nothing)]) -> (IO.isDirectory path >>= bool (IO.readBlobsFromPaths paths) (IO.readBlobsFromDir path) >>= yield) `catchError` (pure . Left . toException)
          ReadBlobs (Right paths) -> (IO.readBlobsFromPaths paths >>= yield) `catchError` (pure . Left . toException)
          ReadBlobPairs source -> (either IO.readBlobPairsFromHandle (traverse (traverse (uncurry IO.readFile))) source >>= yield) `catchError` (pure . Left . toException)
          WriteToOutput destination contents -> either B.hPutStr B.writeFile destination contents >>= yield
          WriteLog level message pairs -> queueLogMessage logger level message pairs >>= yield
          WriteStat stat -> queue statter stat >>= yield
          Time statName tags task -> withTiming (queue statter) statName tags (go task) >>= either (pure . Left) yield
          Parse parser blob -> go (runParser options blob parser) >>= either (pure . Left) yield
          Decorate algebra term -> pure (decoratorWithAlgebra algebra term) >>= yield
          Semantic.Task.Diff differ term1 term2 -> pure (differ term1 term2) >>= yield
          Render renderer input -> pure (renderer input) >>= yield
          Distribute tasks -> Async.mapConcurrently go tasks >>= either (pure . Left) yield . sequenceA . withStrategy (parTraversable (parTraversable rseq))
          LiftIO action -> action >>= yield
          Throw err -> pure (Left err)
          Catch during handler -> do
            result <- go during
            case result of
              Left err -> go (handler err) >>= either (pure . Left) yield
              Right a -> yield a) . fmap Right

runParser :: Options -> Blob -> Parser term -> Task term
runParser Options{..} blob@Blob{..} = go
  where
    go :: Parser term -> Task term
    go parser = case parser of
      ASTParser language ->
        time "parse.tree_sitter_ast_parse" languageTag $
          liftIO ((Right <$> parseToAST language blob) `catchError` (pure . Left . toException)) >>= either throwError pure
      AssignmentParser parser assignment -> do
        ast <- go parser `catchError` \ err -> do
          writeStat (Stat.increment "parse.parse_failures" languageTag)
          writeLog Error "failed parsing" (("task", "parse") : blobFields)
          throwError err
        time "parse.assign" languageTag $
          case Assignment.assign blobSource assignment ast of
            Left err -> do
              writeStat (Stat.increment "parse.assign_errors" languageTag)
              writeLog Error (Error.formatError optionsPrintSource (optionsIsTerminal && optionsEnableColour) blob err) (("task", "assign") : blobFields)
              throwError (toException err)
            Right term -> do
              for_ (errors term) $ \ err -> case Error.errorActual err of
                  (Just "ParseError") -> do
                    writeStat (Stat.increment "parse.parse_errors" languageTag)
                    writeLog Warning (Error.formatError optionsPrintSource (optionsIsTerminal && optionsEnableColour) blob err) (("task", "parse") : blobFields)
                  _ -> do
                    writeStat (Stat.increment "parse.assign_warnings" languageTag)
                    writeLog Warning (Error.formatError optionsPrintSource (optionsIsTerminal && optionsEnableColour) blob err) (("task", "assign") : blobFields)
              writeStat (Stat.count "parse.nodes" (length term) languageTag)
              pure term
      TreeSitterParser tslanguage ->
        time "parse.tree_sitter_parse" languageTag $
          liftIO (treeSitterParser tslanguage blob)
      MarkdownParser ->
        time "parse.cmark_parse" languageTag $
          let term = cmarkParser blobSource
          in length term `seq` pure term
    blobFields = ("path", blobPath) : languageTag
    languageTag = maybe [] (pure . (,) ("language" :: String) . show) blobLanguage
    errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Union fs) (Record Assignment.Location) -> [Error.Error String]
    errors = cata $ \ (In a syntax) -> case syntax of
      _ | Just err@Syntax.Error{} <- prj syntax -> [Syntax.unError (sourceSpan a) err]
      _ -> fold syntax

instance MonadIO Task where
  liftIO action = LiftIO action `Then` return

instance MonadError SomeException Task where
  throwError error = Throw error `Then` return
  catchError during handler = Catch during handler `Then` return
