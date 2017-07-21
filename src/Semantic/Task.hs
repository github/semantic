{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Semantic.Task
( Task
, RAlgebra
, Message(..)
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
, runTask
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
import qualified Data.Syntax as Syntax
import Data.Syntax.Algebra (RAlgebra, decoratorWithAlgebra)
import Data.Syntax.Assignment hiding (Error)
import Data.Union
import Diff
import qualified Files
import Language
import Language.Markdown
import Parser
import Prologue hiding (Location)
import Term
import TreeSitter

data TaskF output where
  ReadBlobs :: Either Handle [(FilePath, Maybe Language)] -> TaskF [Blob]
  ReadBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> TaskF [Both Blob]
  WriteToOutput :: Either Handle FilePath -> ByteString -> TaskF ()
  WriteLog :: Message -> TaskF ()
  Parse :: Parser term -> Blob -> TaskF term
  Decorate :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> TaskF (Term f (Record (field ': fields)))
  Diff :: Differ f a -> Both (Term f a) -> TaskF (Diff f a)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)
  LiftIO :: IO a -> TaskF a

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type Task = Freer TaskF

-- | A log message at a specific level.
data Message
  = Error { messageContent :: ByteString }
  | Warning { messageContent :: ByteString }
  | Info { messageContent :: ByteString }
  | Debug { messageContent :: ByteString }
  deriving (Eq, Show)

formatMessage :: Message -> ByteString
formatMessage (Error s) = "error: " <> s <> "\n"
formatMessage (Warning s) = "warning: " <> s <> "\n"
formatMessage (Info s) = "info: " <> s <> "\n"
formatMessage (Debug s) = "debug: " <> s <> "\n"

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
writeLog :: Message -> Task ()
writeLog message = WriteLog message `Then` return


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


-- | Execute a 'Task', yielding its result value in 'IO'.
runTask :: Task a -> IO a
runTask task = do
  logQueue <- newTMQueueIO
  logging <- async (sink logQueue)

  result <- runFreerM (\ task -> case task of
    ReadBlobs source -> pure <$ writeLog (Info "ReadBlobs") <*> either Files.readBlobsFromHandle (traverse (uncurry Files.readFile)) source
    ReadBlobPairs source -> pure <$ writeLog (Info "ReadBlobPairs") <*> either Files.readBlobPairsFromHandle (traverse (traverse (uncurry Files.readFile))) source
    WriteToOutput destination contents -> pure <$ writeLog (Info "WriteToOutput") <*> liftIO (either B.hPutStr B.writeFile destination contents)
    WriteLog message -> pure <$> liftIO (atomically (writeTMQueue logQueue message))
    Parse parser blob -> pure <$ writeLog (Info "Parse") <*> runParser parser blob
    Decorate algebra term -> pure <$ writeLog (Info "Decorate") <*> pure (decoratorWithAlgebra algebra term)
    Diff differ terms -> pure <$ writeLog (Info "Diff") <*> pure (differ terms)
    Render renderer input -> pure <$ writeLog (Info "Render") <*> pure (renderer input)
    Distribute tasks -> pure <$ writeLog (Info "Distribute") <*> liftIO (Async.mapConcurrently runTask tasks >>= pure . withStrategy (parTraversable rseq))
    LiftIO action -> pure action)
    task
  atomically (closeTMQueue logQueue)
  wait logging
  pure result
  where sink queue = do
          message <- atomically (readTMQueue queue)
          case message of
            Just message -> do
              B.hPutStr stderr (formatMessage message)
              sink queue
            _ -> pure ()

runParser :: Parser term -> Blob -> Task term
runParser parser blob@Blob{..} = case parser of
  ASTParser language -> liftIO $ parseToAST language blobSource
  AssignmentParser parser by assignment -> do
    ast <- runParser parser blob
    case assignBy by assignment blobSource ast of
      Left err -> do
        options <- liftIO $ optionsForHandle stderr
        writeLog (Warning (formatErrorWithOptions options blob err))
        pure (errorTerm blobSource)
      Right term -> pure term
  TreeSitterParser language tslanguage -> liftIO $ treeSitterParser language tslanguage blobSource
  MarkdownParser -> pure (cmarkParser blobSource)
  LineByLineParser -> pure (lineByLineParser blobSource)

errorTerm :: Syntax.Error :< fs => Source -> Term (Union fs) (Record Location)
errorTerm source = cofree ((totalRange source :. totalSpan source :. Nil) :< inj (Syntax.Error []))


instance MonadIO Task where
  liftIO action = LiftIO action `Then` return
