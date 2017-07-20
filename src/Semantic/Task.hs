{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
module Semantic.Task
( Task
, RAlgebra
, Differ
, readBlobs
, readBlobPairs
, writeToOutput
, parse
, decorate
, diff
, render
, distribute
, distributeFor
, distributeFoldMap
, runTask
) where

import qualified Files
import Control.Parallel.Strategies
import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Blob
import qualified Data.ByteString as B
import Data.Functor.Both as Both
import Data.Record
import Data.Syntax.Algebra (RAlgebra, decoratorWithAlgebra)
import Diff
import Language
import Parser
import Prologue
import Term

data TaskF output where
  ReadBlobs :: Either Handle [(FilePath, Maybe Language)] -> TaskF [Blob]
  ReadBlobPairs :: Either Handle [Both (FilePath, Maybe Language)] -> TaskF [Both Blob]
  WriteToOutput :: Either Handle FilePath -> ByteString -> TaskF ()
  Parse :: Parser term -> Blob -> TaskF term
  Decorate :: Functor f => RAlgebra (TermF f (Record fields)) (Term f (Record fields)) field -> Term f (Record fields) -> TaskF (Term f (Record (field ': fields)))
  Diff :: Differ f a -> Both (Term f a) -> TaskF (Diff f a)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)

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

-- | A 'Task' which writes a 'ByteString' to a 'Handle' or a 'FilePath'.
writeToOutput :: Either Handle FilePath -> ByteString -> Task ()
writeToOutput path contents = WriteToOutput path contents `Then` return


-- | A 'Task' which parses 'Source' with the given 'Parser'.
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
runTask = iterFreerA $ \ task yield -> case task of
  ReadBlobs source -> either Files.readBlobsFromHandle (traverse (uncurry Files.readFile)) source >>= yield
  ReadBlobPairs source -> either Files.readBlobPairsFromHandle (traverse (traverse (uncurry Files.readFile))) source >>= yield
  WriteToOutput destination contents -> either B.hPutStr B.writeFile destination contents >>= yield
  Parse parser blob -> runParser parser blob >>= yield
  Decorate algebra term -> yield (decoratorWithAlgebra algebra term)
  Diff differ terms -> yield (differ terms)
  Render renderer input -> yield (renderer input)
  Distribute tasks -> Async.mapConcurrently runTask tasks >>= yield . withStrategy (parTraversable rseq)
