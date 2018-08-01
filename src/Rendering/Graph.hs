{-# LANGUAGE FunctionalDependencies, MonoLocalBinds #-}
module Rendering.Graph
( renderTreeGraph
, termStyle
, diffStyle
, ToTreeGraph(..)
, Vertex(..)
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
import Data.Semigroup.App
import Data.String (IsString(..))
import Data.Term
import Prologue

renderTreeGraph :: (Ord vertex, Recursive t, ToTreeGraph vertex (Base t)) => t -> Graph vertex
renderTreeGraph = simplify . runGraph . cata toTreeGraph

runGraph :: Eff '[Fresh, Reader (Graph vertex)] (Graph vertex) -> Graph vertex
runGraph = run . runReader mempty . runFresh 0


termAlgebra :: (ConstructorName syntax, Foldable syntax, Member Fresh effs, Member (Reader (Graph (Vertex tag))) effs)
            => tag
            -> TermF syntax ann (Eff effs (Graph (Vertex tag)))
            -> Eff effs (Graph (Vertex tag))
termAlgebra tag (In _ syntax) = do
  i <- fresh
  let root = vertex (Vertex i tag (constructorName syntax))
  parent <- ask
  (parent `connect` root <>) <$> local (const root) (runAppMerge (foldMap AppMerge syntax))


style :: (IsString string, Monoid string) => String -> (tag -> [Attribute string]) -> Style (Vertex tag) string
style name tagAttributes = (defaultStyle (fromString . show . vertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes Vertex{..} = "label" := fromString vertexName : tagAttributes vertexTag

termStyle :: (IsString string, Monoid string) => String -> Style (Vertex ()) string
termStyle name = style name (const [])

diffStyle :: (IsString string, Monoid string) => String -> Style (Vertex DiffTag) string
diffStyle name = style name diffTagAttributes
  where diffTagAttributes Deleted  = ["color" := "red"]
        diffTagAttributes Inserted = ["color" := "green"]
        diffTagAttributes _        = []

data Vertex tag = Vertex { vertexId :: Int, vertexTag :: tag, vertexName :: String }
  deriving (Eq, Ord, Show)

data DiffTag = Deleted | Inserted | Merged
  deriving (Eq, Ord, Show)


class ToTreeGraph vertex t | t -> vertex where
  toTreeGraph :: (Member Fresh effs, Member (Reader (Graph vertex)) effs) => t (Eff effs (Graph vertex)) -> Eff effs (Graph vertex)

instance (ConstructorName syntax, Foldable syntax) => ToTreeGraph (Vertex ()) (TermF syntax ann) where
  toTreeGraph = termAlgebra ()

instance (ConstructorName syntax, Foldable syntax) => ToTreeGraph (Vertex DiffTag) (DiffF syntax ann1 ann2) where
  toTreeGraph d = case d of
    Merge t               -> termAlgebra Merged t
    Patch (Delete  t1)    ->          termAlgebra Deleted t1
    Patch (Insert     t2) ->                                     termAlgebra Inserted t2
    Patch (Replace t1 t2) -> (<>) <$> termAlgebra Deleted t1 <*> termAlgebra Inserted t2
