{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, MonoLocalBinds, OverloadedStrings #-}
module Rendering.Graph
( renderTreeGraph
, termStyle
, diffStyle
, ToTreeGraph(..)
) where

import Algebra.Graph.Export.Dot
import Analysis.ConstructorName
import Control.Effect.Fresh
import Control.Effect.Pure
import Control.Effect.Reader
import Control.Effect.State
import Control.Lens
import Data.Diff
import Data.Edit
import Data.Graph
import Data.ProtoLens (defMessage)
import Data.String (IsString (..))
import Data.Term
import Prologue
import Semantic.Api.Bridge
import Proto.Semantic as P
import Proto.Semantic_Fields as P
import Source.Loc as Loc

import qualified Data.Text as T

-- TODO: rename as this isn't a render
renderTreeGraph :: (Ord vertex, Recursive t, ToTreeGraph vertex (Base t)) => t -> Graph vertex
renderTreeGraph = simplify . runGraph . cata toTreeGraph

runGraph :: ReaderC (Graph vertex)
           (FreshC PureC) (Graph vertex)
         -> Graph vertex
runGraph = run . runFresh' . runReader mempty
  where
    -- NB: custom runFresh so that we count starting at 1 in order to avoid
    -- default values for proto encoding.
    runFresh' = evalState 1 . runFreshC

-- | GraphViz styling for terms
termStyle :: (IsString string, Monoid string) => String -> Style TermVertex string
termStyle name = (defaultStyle (fromString . show . view vertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes v = ["label" := fromString (T.unpack (v^.term))]

-- | Graphviz styling for diffs
diffStyle :: (IsString string, Monoid string) => String -> Style DiffTreeVertex string
diffStyle name = (defaultStyle (fromString . show . view diffVertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes v = case v^.maybe'diffTerm of
          Just (DiffTreeVertex'Deleted x)  -> [ "label" := fromString (T.unpack (x^.term)),  "color" := "red" ]
          Just (DiffTreeVertex'Inserted x) -> [ "label" := fromString (T.unpack (x^.term)),  "color" := "green" ]
          Just (DiffTreeVertex'Replaced _) -> [ "label" := "Replacement",                    "color" := "orange", "style" := "dashed" ]
          Just (DiffTreeVertex'Merged x)   -> [ "label" := fromString (T.unpack (x^.term)) ]
          _                                -> []

class ToTreeGraph vertex t | t -> vertex where
  toTreeGraph :: (Member Fresh sig, Member (Reader (Graph vertex)) sig, Carrier sig m) => t (m (Graph vertex)) -> m (Graph vertex)

instance (ConstructorName syntax, Foldable syntax) =>
  ToTreeGraph TermVertex (TermF syntax Loc) where
  toTreeGraph = termAlgebra where
    termAlgebra ::
      ( ConstructorName syntax
      , Foldable syntax
      , Member Fresh sig
      , Member (Reader (Graph TermVertex)) sig
      , Carrier sig m
      )
      => TermF syntax Loc (m (Graph TermVertex))
      -> m (Graph TermVertex)
    termAlgebra (In ann syntax) = do
      i <- fresh
      parent <- ask
      let root = vertex $ defMessage
            & P.vertexId .~ fromIntegral i
            & P.term .~ T.pack (constructorName syntax)
            & P.maybe'span .~ (converting #? Loc.span ann)
      subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
      pure (parent `connect` root `overlay` subGraph)

instance (ConstructorName syntax, Foldable syntax) =>
  ToTreeGraph DiffTreeVertex (DiffF syntax Loc Loc) where
  toTreeGraph d = case d of
    Merge t@(In (a1, a2) syntax)     -> diffAlgebra t . DiffTreeVertex'Merged $ defMessage
                                          & P.term .~ T.pack (constructorName syntax)
                                          & P.maybe'beforeSpan .~ ann a1
                                          & P.maybe'afterSpan .~ ann a2
    Patch (Delete t1@(In a1 syntax)) -> diffAlgebra t1 . DiffTreeVertex'Deleted $ defMessage
                                          & P.term .~ T.pack (constructorName syntax)
                                          & P.maybe'span .~ ann a1
    Patch (Insert t2@(In a2 syntax)) -> diffAlgebra t2 . DiffTreeVertex'Inserted $ defMessage
                                          & P.term .~ T.pack (constructorName syntax)
                                          & P.maybe'span .~ ann a2
    Patch (Compare t1@(In a1 syntax1) t2@(In a2 syntax2)) -> do
      i <- fresh
      parent <- ask
      let (beforeName, beforeSpan) = (T.pack (constructorName syntax1), ann a1)
      let (afterName,  afterSpan) = (T.pack (constructorName syntax2), ann a2)
      let replace = vertex $ defMessage
            & P.diffVertexId .~ fromIntegral i
            & P.maybe'replaced ?~ (defMessage
                                     & P.beforeTerm .~ beforeName
                                     & P.maybe'beforeSpan .~ beforeSpan
                                     & P.afterTerm .~ afterName
                                     & P.maybe'afterSpan .~ afterSpan)
      graph <- local (const replace) (overlay <$> diffAlgebra t1 (DiffTreeVertex'Deleted (defMessage & P.term .~ beforeName & P.maybe'span .~ beforeSpan)) <*> diffAlgebra t2 (DiffTreeVertex'Inserted (defMessage & P.term .~ afterName & P.maybe'span .~ afterSpan)))
      pure (parent `connect` replace `overlay` graph)
    where
      ann a = converting #? Loc.span a
      diffAlgebra ::
        ( Foldable f
        , Member Fresh sig
        , Member (Reader (Graph DiffTreeVertex)) sig
        , Carrier sig m
        ) => f (m (Graph DiffTreeVertex)) -> DiffTreeVertex'DiffTerm -> m (Graph DiffTreeVertex)
      diffAlgebra syntax a = do
        i <- fresh
        parent <- ask
        let root = vertex $ defMessage
              & P.diffVertexId .~ fromIntegral i
              & P.maybe'diffTerm ?~ a
        subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
        pure (parent `connect` root `overlay` subGraph)
