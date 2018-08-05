{-# LANGUAGE FunctionalDependencies, MonoLocalBinds #-}
module Rendering.Graph
( renderTreeGraph
, termStyle
, diffStyle
, ToTreeGraph(..)
, TaggedVertex(..)
, DiffTag(..)
) where

import Algebra.Graph.Export.Dot
import Analysis.ConstructorName
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import Data.Diff
import Data.Graph
import Data.Patch
import Data.String (IsString(..))
import Data.Term
import Prologue

renderTreeGraph :: (Ord vertex, Recursive t, ToTreeGraph vertex (Base t)) => t -> Graph vertex
renderTreeGraph = simplify . runGraph . cata toTreeGraph

runGraph :: Eff '[Reader (Graph vertex), Fresh] (Graph vertex) -> Graph vertex
runGraph = run . runFresh 0 . runReader mempty


termAlgebra :: (ConstructorName syntax, Foldable syntax, Member Fresh effs, Member (Reader (Graph (TaggedVertex tag))) effs)
            => tag
            -> TermF syntax ann (Eff effs (Graph (TaggedVertex tag)))
            -> Eff effs (Graph (TaggedVertex tag))
termAlgebra t (In _ syntax) = do
  i <- fresh
  parent <- ask
  let root = vertex (TaggedVertex i t (constructorName syntax))
  subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
  pure (parent `connect` root `overlay` subGraph)

style :: (IsString string, Monoid string) => String -> (tag -> [Attribute string]) -> Style (TaggedVertex tag) string
style name tagAttributes = (defaultStyle (fromString . show . vertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes TaggedVertex{..} = "label" := fromString vertexName : tagAttributes vertexTag

termStyle :: (IsString string, Monoid string) => String -> Style (TaggedVertex ()) string
termStyle name = style name (const [])

diffStyle :: (IsString string, Monoid string) => String -> Style (TaggedVertex DiffTag) string
diffStyle name = style name diffTagAttributes
  where diffTagAttributes Deleted  = ["color" := "red"]
        diffTagAttributes Inserted = ["color" := "green"]
        diffTagAttributes Replaced = ["color" := "orange", "style" := "dashed"]
        diffTagAttributes _        = []

data TaggedVertex tag = TaggedVertex { vertexId :: Int, vertexTag :: tag, vertexName :: String }
  deriving (Eq, Ord, Show)

data DiffTag = Deleted | Inserted | Merged | Replaced
  deriving (Eq, Ord, Show)


class ToTreeGraph vertex t | t -> vertex where
  toTreeGraph :: (Member Fresh effs, Member (Reader (Graph vertex)) effs) => t (Eff effs (Graph vertex)) -> Eff effs (Graph vertex)

instance (ConstructorName syntax, Foldable syntax) => ToTreeGraph (TaggedVertex ()) (TermF syntax ann) where
  toTreeGraph = termAlgebra ()

instance (ConstructorName syntax, Foldable syntax) => ToTreeGraph (TaggedVertex DiffTag) (DiffF syntax ann1 ann2) where
  toTreeGraph d = case d of
    Merge t               -> termAlgebra Merged t
    Patch (Delete  t1)    ->          termAlgebra Deleted t1
    Patch (Insert     t2) ->                                     termAlgebra Inserted t2
    Patch (Replace t1 t2) -> do
      i <- fresh
      parent <- ask
      let replace = vertex (TaggedVertex i Replaced "Replacement")
      graph <- local (const replace) (overlay <$> termAlgebra Deleted t1 <*> termAlgebra Inserted t2)
      pure (parent `connect` replace `overlay` graph)
