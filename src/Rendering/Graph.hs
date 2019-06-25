{-# LANGUAGE FunctionalDependencies, MonoLocalBinds #-}
module Rendering.Graph
( renderTreeGraph
, termStyle
, diffStyle
, ToTreeGraph(..)
) where

import Algebra.Graph.Export.Dot
import Analysis.ConstructorName
import Control.Effect
import Control.Effect.Fresh
import Control.Effect.Reader
import Control.Effect.State
import Data.Diff
import Data.Graph
import Data.Location
import Data.Patch
import Data.String (IsString (..))
import Data.Term
import Prologue
import Semantic.Api.Bridge
import Semantic.Api.V1.CodeAnalysisPB

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
termStyle name = (defaultStyle (fromString . show . vertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes TermVertex{..} = ["label" := fromString name]

-- | Graphviz styling for diffs
diffStyle :: (IsString string, Monoid string) => String -> Style DiffTreeVertex string
diffStyle name = (defaultStyle (fromString . show . diffVertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes (DiffTreeVertex _ (Just (Deleted  DeletedTerm{..})))  = [ "label" := fromString (T.unpack term),  "color" := "red" ]
        vertexAttributes (DiffTreeVertex _ (Just (Inserted InsertedTerm{..}))) = [ "label" := fromString (T.unpack term), "color" := "green" ]
        vertexAttributes (DiffTreeVertex _ (Just (Replaced ReplacedTerm{..}))) = [ "label" := "Replacement",               "color" := "orange", "style" := "dashed" ]
        vertexAttributes (DiffTreeVertex _ (Just (Merged MergedTerm{..})))     = [ "label" := fromString (T.unpack term) ]
        vertexAttributes _ = []

class ToTreeGraph vertex t | t -> vertex where
  toTreeGraph :: (Member Fresh sig, Member (Reader (Graph vertex)) sig, Carrier sig m) => t (m (Graph vertex)) -> m (Graph vertex)

instance (ConstructorName syntax, Foldable syntax) =>
  ToTreeGraph TermVertex (TermF syntax Location) where
  toTreeGraph = termAlgebra where
    termAlgebra ::
      ( ConstructorName syntax
      , Foldable syntax
      , Member Fresh sig
      , Member (Reader (Graph TermVertex)) sig
      , Carrier sig m
      )
      => TermF syntax Location (m (Graph TermVertex))
      -> m (Graph TermVertex)
    termAlgebra (In ann syntax) = do
      i <- fresh
      parent <- ask
      let root = vertex $ TermVertex (fromIntegral i) (T.pack (constructorName syntax)) (converting #? locationSpan ann)
      subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
      pure (parent `connect` root `overlay` subGraph)

instance (ConstructorName syntax, Foldable syntax) =>
  ToTreeGraph DiffTreeVertex (DiffF syntax Location Location) where
  toTreeGraph d = case d of
    Merge t@(In (a1, a2) syntax)     -> diffAlgebra t  (Merged   (MergedTerm (T.pack (constructorName syntax)) (ann a1) (ann a2)))
    Patch (Delete t1@(In a1 syntax)) -> diffAlgebra t1 (Deleted  (DeletedTerm (T.pack (constructorName syntax)) (ann a1)))
    Patch (Insert t2@(In a2 syntax)) -> diffAlgebra t2 (Inserted (InsertedTerm (T.pack (constructorName syntax)) (ann a2)))
    Patch (Replace t1@(In a1 syntax1) t2@(In a2 syntax2)) -> do
      i <- fresh
      parent <- ask
      let (beforeName, beforeSpan) = (T.pack (constructorName syntax1), ann a1)
      let (afterName,  afterSpan) = (T.pack (constructorName syntax2), ann a2)
      let replace = vertex (DiffTreeVertex (fromIntegral i) (Just (Replaced (ReplacedTerm beforeName beforeSpan afterName afterSpan))))
      graph <- local (const replace) (overlay <$> diffAlgebra t1 (Deleted (DeletedTerm beforeName beforeSpan))) <*> diffAlgebra t2 (Inserted (InsertedTerm afterName afterSpan))
      pure (parent `connect` replace `overlay` graph)
    where
      ann a = converting #? locationSpan a
      diffAlgebra ::
        ( Foldable f
        , Member Fresh sig
        , Member (Reader (Graph DiffTreeVertex)) sig
        , Carrier sig m
        ) => f (m (Graph DiffTreeVertex)) -> DiffTreeVertexDiffTerm -> m (Graph DiffTreeVertex)
      diffAlgebra syntax a = do
        i <- fresh
        parent <- ask
        let root = vertex (DiffTreeVertex (fromIntegral i) (Just a))
        subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
        pure (parent `connect` root `overlay` subGraph)
