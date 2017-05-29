{-# LANGUAGE GADTs #-}
module Semantic.Task
( Task
, Decorator
, Differ
, NamedDecorator(..)
, DiffRenderer(..)
, TermRenderer(..)
, parse
, decorate
, diff
, render
, distribute
, distributeFor
, runTask
) where

import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Aeson (Value)
import Data.Functor.Both as Both
import Diff
import Parser
import Prologue
import Renderer
import Source
import Term

data TaskF output where
  Parse :: Parser term -> Source -> TaskF term
  Decorate :: Decorator term term' -> Source -> term -> TaskF term'
  Diff :: Differ f a -> Both (Term f a) -> TaskF (Diff f a)
  Render :: Renderer input output -> input -> TaskF output
  Distribute :: Traversable t => t (Task output) -> TaskF (t output)

type Task = Freer TaskF

type Decorator input output = Source -> input -> output

type Differ f a = Both (Term f a) -> Diff f a


data NamedDecorator = IdentifierDecorator | IdentityDecorator

data DiffRenderer output where
  JSONDiffRenderer :: DiffRenderer [Value]
  SExpressionDiffRenderer :: DiffRenderer ByteString

data TermRenderer output where
  JSONTermRenderer :: TermRenderer [Value]
  SExpressionTermRenderer :: TermRenderer ByteString

parse :: Parser term -> Source -> Task term
parse parser source = Parse parser source `Then` return

decorate :: Decorator term term' -> Source -> term -> Task term'
decorate decorator source term = Decorate decorator source term `Then` return

diff :: Differ f a -> Both (Term f a) -> Task (Diff f a)
diff differ terms = Diff differ terms `Then` return

render :: Renderer input output -> input -> Task output
render renderer input = Render renderer input `Then` return

distribute :: Traversable t => t (Task output) -> Task (t output)
distribute tasks = Distribute tasks `Then` return

distributeFor :: Traversable t => t a -> (a -> Task output) -> Task (t output)
distributeFor inputs toTask = Distribute (fmap toTask inputs) `Then` return


runTask :: Task a -> IO a
runTask = iterFreerA $ \ task yield -> case task of
  Parse parser source -> runParser parser source >>= yield
  Decorate decorator source term -> yield (decorator source term)
  Diff differ terms -> yield (differ terms)
  Render renderer input -> yield (runRenderer renderer input)
  Distribute tasks -> Async.mapConcurrently runTask tasks >>= yield
