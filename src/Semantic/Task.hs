{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Semantic.Task
( Task
, Level(..)
, RAlgebra
, Differ
, readBlobs
, readBlobPairs
, writeToOutput
, writeLog
, parse
, decorate
, diff
, render
, distribute
, distributeFor
, distributeFoldMap
, Options(..)
, defaultOptions
, configureOptionsForHandle
, runTask
, runTaskWithOptions
) where

import Control.Concurrent.STM.TMQueue
import Control.Monad.IO.Class
import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Blob
import qualified Data.ByteString as B
import Data.Functor.Both as Both
import Data.Record
import Data.Source
import Data.String
import qualified Data.Syntax as Syntax
import Data.Syntax.Algebra (RAlgebra, decoratorWithAlgebra)
import qualified Data.Syntax.Assignment as Assignment
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time (getCurrentTime)
import Data.Union
import Diff
import qualified Files
import Language
import Language.Markdown
import Parser
import Prologue hiding (Location)
import System.Console.ANSI
import System.IO (hIsTerminalDevice, hPutStr)
import Term
import Text.Show
import TreeSitter

data TaskF output where
  ReadBlobs :: Either Handle [(FilePath, Maybe Language)] -> TaskF [Blob]
  ReadBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> TaskF [Both Blob]
  WriteToOutput :: Either Handle FilePath -> ByteString -> TaskF ()
  WriteLog :: Level -> String -> TaskF ()
  Parse :: Parser term -> Blob -> TaskF term
  Decorate :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> TaskF (Term f (Record (field ': fields)))
  Diff :: Differ f a -> Both (Term f a) -> TaskF (Diff f a)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)
  LiftIO :: IO a -> TaskF a

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type Task = Freer TaskF

-- | A log message at a specific level.
data Message = Message Time.UTCTime Level String
  deriving (Eq, Show)

data Level
  = Error
  | Warning
  | Info
  | Debug
  deriving (Eq, Ord, Show)

-- | Format a 'Message', optionally colourized.
formatMessage :: Bool -> Message -> String
formatMessage colourize (Message _ level message) = showLevel level . showString ": " . showString message . showChar '\n' $ ""
  where showLevel Error = Assignment.withSGRCode colourize [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity] (showString "error")
        showLevel Warning = Assignment.withSGRCode colourize [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity] (showString "warning")
        showLevel Info = Assignment.withSGRCode colourize [SetConsoleIntensity BoldIntensity] (showString "info")
        showLevel Debug = Assignment.withSGRCode colourize [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity] (showString "debug")


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

-- | A 'Task' which writes a 'ByteString' to a 'Handle' or a 'FilePath'.
writeToOutput :: Either Handle FilePath -> ByteString -> Task ()
writeToOutput path contents = WriteToOutput path contents `Then` return


-- | A 'Task' which logs a message at a specific log level to stderr.
writeLog :: Level -> String -> Task ()
writeLog level message = WriteLog level message `Then` return


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

-- | Options controlling 'Task' logging, error handling, &c.
data Options = Options
  { optionsColour :: Maybe Bool -- ^ Whether to use colour formatting for errors. 'Nothing' implies automatic selection for the stderr handle, using colour for terminal handles but not for regular files.
  , optionsLevel :: Maybe Level -- ^ What level of messages to log. 'Nothing' disabled logging.
  , optionsPrintSource :: Bool -- ^ Whether to print the source reference when logging errors.
  }

defaultOptions :: Options
defaultOptions = Options
  { optionsColour = Nothing
  , optionsLevel = Just Warning
  , optionsPrintSource = False
  }

configureOptionsForHandle :: Handle -> Options -> IO Options
configureOptionsForHandle handle options = do
  isTerminal <- hIsTerminalDevice handle
  pure $ options
    { optionsColour = optionsColour options <|> Just isTerminal
    }

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
  logging <- async (logSink options logQueue)

  result <- runFreerM (\ task -> case task of
    ReadBlobs source -> pure <$ writeLog Info "ReadBlobs" <*> either Files.readBlobsFromHandle (traverse (uncurry Files.readFile)) source
    ReadBlobPairs source -> pure <$ writeLog Info "ReadBlobPairs" <*> either Files.readBlobPairsFromHandle (traverse (traverse (uncurry Files.readFile))) source
    WriteToOutput destination contents -> pure <$ writeLog Info "WriteToOutput" <*> liftIO (either B.hPutStr B.writeFile destination contents)
    WriteLog level message
      | Just logLevel <- optionsLevel options, level <= logLevel -> pure <$> liftIO (do
        now <- Time.getCurrentTime
        atomically (writeTMQueue logQueue (Message now level message)))
      | otherwise -> pure (pure ())
    Parse parser blob -> pure <$ writeLog Info "Parse" <*> runParser options parser blob
    Decorate algebra term -> pure <$ writeLog Info "Decorate" <*> pure (decoratorWithAlgebra algebra term)
    Diff differ terms -> pure <$ writeLog Info "Diff" <*> pure (differ terms)
    Render renderer input -> pure <$ writeLog Info "Render" <*> pure (renderer input)
    Distribute tasks -> pure <$ writeLog Info "Distribute" <*> liftIO (Async.mapConcurrently runTask tasks >>= pure . withStrategy (parTraversable rseq))
    LiftIO action -> pure action)
    task
  atomically (closeTMQueue logQueue)
  wait logging
  pure result
  where logSink options queue = do
          message <- atomically (readTMQueue queue)
          case message of
            Just (Message _ level message) -> do
              hPutStr stderr (formatMessage (fromMaybe True (optionsColour options)) level message)
              logSink options queue
            _ -> pure ()

runParser :: Options -> Parser term -> Blob -> Task term
runParser options parser blob@Blob{..} = case parser of
  ASTParser language -> liftIO $ parseToAST language blobSource
  AssignmentParser parser by assignment -> do
    ast <- runParser options parser blob
    case Assignment.assignBy by blobSource assignment ast of
      Left err -> do
        let formatOptions = Assignment.defaultOptions
              { Assignment.optionsColour = fromMaybe True (optionsColour options)
              , Assignment.optionsIncludeSource = optionsPrintSource options
              }
        writeLog Warning (Assignment.formatErrorWithOptions formatOptions blob err)
        pure (errorTerm blobSource)
      Right term -> pure term
  TreeSitterParser language tslanguage -> liftIO $ treeSitterParser language tslanguage blobSource
  MarkdownParser -> pure (cmarkParser blobSource)
  LineByLineParser -> pure (lineByLineParser blobSource)

errorTerm :: Syntax.Error :< fs => Source -> Term (Union fs) (Record Assignment.Location)
errorTerm source = cofree ((totalRange source :. totalSpan source :. Nil) :< inj (Syntax.Error []))


instance MonadIO Task where
  liftIO action = LiftIO action `Then` return
