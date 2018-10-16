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
import Data.Diff
import Data.Graph
import Data.Graph.TermVertex
import Data.Graph.DiffVertex
import Data.Location
import Data.Patch
import Data.String (IsString(..))
import Data.Term
import Prologue

-- TODO: rename as this isn't a render
renderTreeGraph :: (Ord vertex, Recursive t, ToTreeGraph vertex (Base t)) => t -> Graph vertex
renderTreeGraph = simplify . runGraph . cata toTreeGraph

runGraph :: Eff (ReaderC (Graph vertex)
           (Eff (FreshC
           (Eff VoidC)))) (Graph vertex)
         -> Graph vertex
runGraph = run . runFresh . runReader mempty

-- | GraphViz styling for terms
termStyle :: (IsString string, Monoid string) => String -> Style TermVertex string
termStyle name = (defaultStyle (fromString . show . vertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes TermVertex{..} = ["label" := fromString vertexTermName]

-- | Graphviz styling for diffs
diffStyle :: (IsString string, Monoid string) => String -> Style DiffVertex string
diffStyle name = (defaultStyle (fromString . show . diffVertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes (DiffVertex _ (Deleted DeletedTerm{..}))   = [ "label" := fromString deletedTermName,  "color" := "red" ]
        vertexAttributes (DiffVertex _ (Inserted InsertedTerm{..})) = [ "label" := fromString insertedTermName, "color" := "green" ]
        vertexAttributes (DiffVertex _ (Replaced ReplacedTerm{..})) = [ "label" := "Replacement",               "color" := "orange", "style" := "dashed" ]
        vertexAttributes (DiffVertex _ (Merged MergedTerm{..}))     = [ "label" := fromString mergedTermName ]

class ToTreeGraph vertex t | t -> vertex where
  toTreeGraph :: (Member Fresh sig, Member (Reader (Graph vertex)) sig, Carrier sig m, Monad m) => t (m (Graph vertex)) -> m (Graph vertex)

instance (ConstructorName syntax, Foldable syntax) =>
  ToTreeGraph TermVertex (TermF syntax Location) where
  toTreeGraph = termAlgebra where
    termAlgebra ::
      ( ConstructorName syntax
      , Foldable syntax
      , Member Fresh sig
      , Member (Reader (Graph TermVertex)) sig
      , Carrier sig m
      , Monad m
      )
      => TermF syntax Location (m (Graph TermVertex))
      -> m (Graph TermVertex)
    termAlgebra (In ann syntax) = do
      i <- fresh
      parent <- ask
      let root = vertex (TermVertex i (constructorName syntax) (TermAnnotation (locationByteRange ann) (locationSpan ann)))
      subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
      pure (parent `connect` root `overlay` subGraph)

instance (ConstructorName syntax, Foldable syntax) =>
  ToTreeGraph DiffVertex (DiffF syntax Location Location) where
  toTreeGraph d = case d of
    Merge t@(In (a1, a2) syntax)     -> diffAlgebra t  (Merged   (MergedTerm (constructorName syntax) (ann a1) (ann a2)))
    Patch (Delete t1@(In a1 syntax)) -> diffAlgebra t1 (Deleted  (DeletedTerm (constructorName syntax) (ann a1)))
    Patch (Insert t2@(In a2 syntax)) -> diffAlgebra t2 (Inserted (InsertedTerm (constructorName syntax) (ann a2)))
    Patch (Replace t1@(In a1 syntax1) t2@(In a2 syntax2)) -> do
      i <- fresh
      parent <- ask
      let a = DeletedTerm (constructorName syntax1) (ann a1)
      let b = InsertedTerm (constructorName syntax2) (ann a2)
      let replace = vertex (DiffVertex i (Replaced (ReplacedTerm a b)))
      graph <- local (const replace) (overlay <$> diffAlgebra t1 (Deleted a) <*> diffAlgebra t2 (Inserted b))
      pure (parent `connect` replace `overlay` graph)
    where
      ann a = TermAnnotation (locationByteRange a) (locationSpan a)
      diffAlgebra ::
        ( Foldable f
        , Member Fresh sig
        , Member (Reader (Graph DiffVertex)) sig
        , Carrier sig m
        , Monad m
        ) => f (m (Graph DiffVertex)) -> DiffVertexTerm -> m (Graph DiffVertex)
      diffAlgebra syntax a = do
        i <- fresh
        parent <- ask
        let root = vertex (DiffVertex i a)
        subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
        pure (parent `connect` root `overlay` subGraph)
