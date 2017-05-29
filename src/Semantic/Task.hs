{-# LANGUAGE GADTs #-}
module Semantic.Task
( Task
, Decorator
, Differ
, NamedDecorator(..)
, NamedRenderer(..)
, parse
, decorate
, diff
, render
, distribute
, distributeFor
, parseAndRenderBlob
, parseDiffAndRenderBlobs
, runTask
) where

import qualified Control.Concurrent.Async as Async
import Control.Monad.Free.Freer
import Data.Aeson (Value)
import Data.Functor.Both as Both
import Data.Record
import Diff
import qualified Info
import Interpreter
import qualified Language
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

data NamedRenderer output where
  JSON :: NamedRenderer [Value]
  SExpression :: NamedRenderer ByteString

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


parseAndRenderBlob :: NamedDecorator -> NamedRenderer output -> SourceBlob -> Task output
parseAndRenderBlob decorator renderer blob@SourceBlob{..} = case blobLanguage of
  Just Language.Python -> do
    term <- parse pythonParser source
    term' <- decorate (case decorator of
      IdentityDecorator -> const identity
      IdentifierDecorator -> const identity) source term
    case renderer of
      JSON -> render JSONRenderer (Identity blob, term')
      SExpression -> render SExpressionParseTreeRenderer (Identity blob, fmap (Info.Other "Term" :. ) term')
  language -> do
    term <- parse (parserForLanguage language) source
    case decorator of
      IdentifierDecorator -> do
        term' <- decorate (const identifierDecorator) source term
        render (case renderer of
          JSON -> JSONRenderer
          SExpression -> SExpressionParseTreeRenderer) (Identity blob, term')
      IdentityDecorator ->
        render (case renderer of
          JSON -> JSONRenderer
          SExpression -> SExpressionParseTreeRenderer) (Identity blob, term)

parseDiffAndRenderBlobs :: NamedDecorator -> NamedRenderer output -> Both SourceBlob -> Task output
parseDiffAndRenderBlobs decorator renderer blobs = do
  let languages = blobLanguage <$> blobs
  terms <- distributeFor blobs $ \ blob -> do
    term <- parse (if runBothWith (==) languages then parserForLanguage (Both.fst languages) else LineByLineParser) (source blob)
    case decorator of
      IdentityDecorator -> pure term
      IdentifierDecorator -> decorate (const identity) (source blob) term
  diffed <- diff (runBothWith diffTerms) terms
  render (case renderer of
    JSON -> JSONRenderer
    SExpression -> SExpressionDiffRenderer) (blobs, diffed)


runTask :: Task a -> IO a
runTask = iterFreerA $ \ task yield -> case task of
  Parse parser source -> runParser parser source >>= yield
  Decorate decorator source term -> yield (decorator source term)
  Diff differ terms -> yield (differ terms)
  Render renderer input -> yield (runRenderer renderer input)
  Distribute tasks -> Async.mapConcurrently runTask tasks >>= yield
