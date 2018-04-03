{-# LANGUAGE GADTs, RankNTypes, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
, bidistribute
, bidistributeFor
, defaultOptions
, configureOptionsForHandle
, terminalFormatter
, logfmtFormatter
, runTask
, runTaskWithOptions
, throwError
) where

import           Analysis.Decorator (decoratorWithAlgebra)
import qualified Assigning.Assignment as Assignment
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as Exc
import           Control.Monad.Effect.Exception
import           Control.Monad.Effect.Internal as Eff
import           Control.Monad.Effect.Reader
import           Control.Monad.IO.Class
import           Control.Parallel.Strategies
import           Data.Blob
import           Data.Bool
import qualified Data.ByteString as B
import           Data.Diff
import qualified Data.Error as Error
import           Data.Language
import           Data.Record
import qualified Data.Syntax as Syntax
import           Data.Term
import           Parsing.CMark
import           Parsing.Parser
import           Parsing.TreeSitter
import           Prologue hiding (MonadError(..))
import qualified Semantic.IO as IO
import           Semantic.Log
import           Semantic.Queue
import           Semantic.Stat as Stat
import           System.Exit (die)
import           System.IO (Handle, stderr)


data TaskF output where
  ReadBlobs :: Either Handle [(FilePath, Maybe Language)] -> TaskF [Blob]
  ReadBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> TaskF [BlobPair]
  WriteToOutput :: Either Handle FilePath -> B.ByteString -> TaskF ()
  WriteLog :: Level -> String -> [(String, String)] -> TaskF ()
  WriteStat :: Stat -> TaskF ()
  Parse :: Parser term -> Blob -> TaskF term
  Decorate :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> TaskF (Term f (Record (field ': fields)))
  Diff :: Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> TaskF (Diff syntax ann1 ann2)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)
  Bidistribute :: Bitraversable t => t (Task output1) (Task output2) -> TaskF (t output1 output2)

type Logger = AsyncQueue Message Options
type Statter = AsyncQueue Stat StatsClient

-- | A high-level task producing some result, e.g. parsing, diffing, rendering. 'Task's can also specify explicit concurrency via 'distribute', 'distributeFor', and 'distributeFoldMap'
type Task = Eff '[TaskF, Reader Options, Reader Logger, Reader Statter, Exc SomeException, IO]

-- | A function to compute the 'Diff' for a pair of 'Term's with arbitrary syntax functor & annotation types.
type Differ syntax ann1 ann2 = Term syntax ann1 -> Term syntax ann2 -> Diff syntax ann1 ann2

-- | A function to render terms or diffs.
type Renderer i o = i -> o

-- | A task which reads a list of 'Blob's from a 'Handle' or a list of 'FilePath's optionally paired with 'Language's.
readBlobs :: Member TaskF effs => Either Handle [(FilePath, Maybe Language)] -> Eff effs [Blob]
readBlobs = send . ReadBlobs

-- | A task which reads a list of pairs of 'Blob's from a 'Handle' or a list of pairs of 'FilePath's optionally paired with 'Language's.
readBlobPairs :: Member TaskF effs => Either Handle [Both (FilePath, Maybe Language)] -> Eff effs [BlobPair]
readBlobPairs = send . ReadBlobPairs

-- | A task which writes a 'B.ByteString' to a 'Handle' or a 'FilePath'.
writeToOutput :: Member TaskF effs => Either Handle FilePath -> B.ByteString -> Eff effs ()
writeToOutput path = send . WriteToOutput path

-- | A task which logs a message at a specific log level to stderr.
writeLog :: Member TaskF effs => Level -> String -> [(String, String)] -> Eff effs ()
writeLog level message = send . WriteLog level message

-- | A task which writes a stat.
writeStat :: Stat -> Task ()
writeStat = send . WriteStat

-- | A task which measures and stats the timing of another task.
time :: (Member (Reader Statter) effs, Member IO effs) => String -> [(String, String)] -> Eff effs output -> Eff effs output
time statName tags task = ask >>= \ statter -> withTiming (liftIO . queue (statter :: Statter)) statName tags task

-- | A task which parses a 'Blob' with the given 'Parser'.
parse :: Member TaskF effs => Parser term -> Blob -> Eff effs term
parse parser = send . Parse parser

-- | A task which decorates a 'Term' with values computed using the supplied 'RAlgebra' function.
decorate :: (Functor f, Member TaskF effs) => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> Eff effs (Term f (Record (field ': fields)))
decorate algebra = send . Decorate algebra

-- | A task which diffs a pair of terms using the supplied 'Differ' function.
diff :: Member TaskF effs => Differ syntax ann1 ann2 -> Term syntax ann1 -> Term syntax ann2 -> Eff effs (Diff syntax ann1 ann2)
diff differ term1 term2 = send (Semantic.Task.Diff differ term1 term2)

-- | A task which renders some input using the supplied 'Renderer' function.
render :: Member TaskF effs => Renderer input output -> input -> Eff effs output
render renderer = send . Render renderer

-- | Distribute a 'Traversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'sequenceA'.
distribute :: (Member TaskF effs, Traversable t) => t (Task output) -> Eff effs (t output)
distribute = send . Distribute

-- | Distribute a 'Bitraversable' container of tasks over the available cores (i.e. execute them concurrently), collecting their results.
--
--   This is a concurrent analogue of 'bisequenceA'.
bidistribute :: (Bitraversable t, Member TaskF effs) => t (Task output1) (Task output2) -> Eff effs (t output1 output2)
bidistribute = send . Bidistribute

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'for' or 'traverse' (with the arguments flipped).
distributeFor :: (Member TaskF effs, Traversable t) => t a -> (a -> Task output) -> Eff effs (t output)
distributeFor inputs toTask = distribute (fmap toTask inputs)

-- | Distribute the application of a function to each element of a 'Bitraversable' container of inputs over the available cores (i.e. perform the functions concurrently for each element), collecting the results.
--
--   This is a concurrent analogue of 'bifor' or 'bitraverse' (with the arguments flipped).
bidistributeFor :: (Bitraversable t, Member TaskF effs) => t a b -> (a -> Task output1) -> (b -> Task output2) -> Eff effs (t output1 output2)
bidistributeFor inputs toTask1 toTask2 = bidistribute (bimap toTask1 toTask2 inputs)

-- | Distribute the application of a function to each element of a 'Traversable' container of inputs over the available cores (i.e. perform the function concurrently for each element), combining the results 'Monoid'ally into a final value.
--
--   This is a concurrent analogue of 'foldMap'.
distributeFoldMap :: (Member TaskF effs, Monoid output, Traversable t) => (a -> Task output) -> t a -> Eff effs output
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
        -> Logger
        -> Statter
        -> Task a
        -> IO (Either SomeException a)
    run options logger statter = run'
      where
        run' :: Task a -> IO (Either SomeException a)
        run' = runM . runError . flip runReader statter . flip runReader logger . flip runReader options . go
        go :: Task a -> Eff '[Reader Options, Reader Logger, Reader Statter, Exc SomeException, IO] a
        go = interpret (\ task -> case task of
          ReadBlobs (Left handle) -> rethrowing (IO.readBlobsFromHandle handle)
          ReadBlobs (Right paths@[(path, Nothing)]) -> rethrowing (IO.isDirectory path >>= bool (IO.readBlobsFromPaths paths) (IO.readBlobsFromDir path))
          ReadBlobs (Right paths) -> rethrowing (IO.readBlobsFromPaths paths)
          ReadBlobPairs source -> rethrowing (either IO.readBlobPairsFromHandle (traverse (runBothWith IO.readFilePair)) source)
          WriteToOutput destination contents -> liftIO (either B.hPutStr B.writeFile destination contents)
          WriteLog level message pairs -> ask >>= \ logger -> queueLogMessage logger level message pairs
          WriteStat stat -> ask >>= \ statter -> liftIO (queue (statter :: Statter) stat)
          Parse parser blob -> ask >>= \ options -> go (runParser options blob parser)
          Decorate algebra term -> pure (decoratorWithAlgebra algebra term)
          Semantic.Task.Diff differ term1 term2 -> pure (differ term1 term2)
          Render renderer input -> pure (renderer input)
          Distribute tasks -> liftIO (Async.mapConcurrently run' tasks) >>= either throwError pure . sequenceA . withStrategy (parTraversable (parTraversable rseq))
          Bidistribute tasks -> liftIO (Async.runConcurrently (bitraverse (Async.Concurrently . run') (Async.Concurrently . run') tasks)) >>= either throwError pure . bisequenceA . withStrategy (parBitraversable (parTraversable rseq) (parTraversable rseq)))

        parBitraversable :: Bitraversable t => Strategy a -> Strategy b -> Strategy (t a b)
        parBitraversable strat1 strat2 = bitraverse (rparWith strat1) (rparWith strat2)

runParser :: Options -> Blob -> Parser term -> Task term
runParser Options{..} blob@Blob{..} = go
  where
    go :: Parser term -> Task term
    go parser = case parser of
      ASTParser language ->
        time "parse.tree_sitter_ast_parse" languageTag $
          rethrowing (parseToAST language blob)
      AssignmentParser parser assignment -> do
        ast <- go parser `catchError` \ (SomeException err) -> do
          writeStat (Stat.increment "parse.parse_failures" languageTag)
          writeLog Error "failed parsing" (("task", "parse") : blobFields)
          throwError (toException err)
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
      MarkdownParser ->
        time "parse.cmark_parse" languageTag $
          let term = cmarkParser blobSource
          in length term `seq` pure term
    blobFields = ("path", blobPath) : languageTag
    languageTag = maybe [] (pure . (,) ("language" :: String) . show) blobLanguage
    errors :: (Syntax.Error :< fs, Apply Foldable fs, Apply Functor fs) => Term (Union fs) (Record Assignment.Location) -> [Error.Error String]
    errors = cata $ \ (In a syntax) -> case syntax of
      _ | Just err@Syntax.Error{} <- prj syntax -> [Syntax.unError (getField a) err]
      _ -> fold syntax


-- | Catch exceptions in 'IO' actions embedded in 'Eff', handling them with the passed function.
--
--   Note that while the type allows 'IO' to occur anywhere within the effect list, it must actually occur at the end to be able to run the computation.
catchException :: ( Exc.Exception e
                  , Member IO r
                  )
               => Eff r a
               -> (e -> Eff r a)
               -> Eff r a
catchException m handler = interpose pure (\ m yield -> send (Exc.try m) >>= either handler yield) m

-- | Lift an 'IO' action into 'Eff', catching and rethrowing any exceptions it throws into an 'Exc' effect.
rethrowing :: ( Member (Exc SomeException) r
              , Member IO r
              )
           => IO a
           -> Eff r a
rethrowing m = catchException (liftIO m) (throwError . toException @SomeException)

-- | Handle the topmost effect by interpreting it into the underlying effects.
interpret :: (forall a. eff a -> Eff effs a) -> Eff (eff ': effs) a -> Eff effs a
interpret f = relay pure (\ eff yield -> f eff >>= yield)

instance Member IO effs => MonadIO (Eff effs) where
  liftIO = send
