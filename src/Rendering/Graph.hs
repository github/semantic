{-# LANGUAGE FunctionalDependencies, MonoLocalBinds #-}
module Rendering.Graph
( renderTreeGraph
, termStyle
, diffStyle
, ToTreeGraph(..)
, TaggedVertex(..)
, DiffTag(..)
) where

import Data.Aeson
import Data.JSON.Fields
import Algebra.Graph.Export.Dot
import Analysis.ConstructorName
import Control.Monad.Effect
import Control.Monad.Effect.Fresh
import Control.Monad.Effect.Reader
import qualified Data.Text as T
import Data.Diff
import Data.Graph
import Data.Range
import Data.Span
import Data.Record
import Data.Patch
import Data.String (IsString(..))
import Data.Term
import Prologue

renderTreeGraph :: (Ord vertex, Recursive t, ToTreeGraph vertex (Base t)) => t -> Graph vertex
renderTreeGraph = simplify . runGraph . cata toTreeGraph

runGraph :: Eff '[Reader (Graph vertex), Fresh] (Graph vertex) -> Graph vertex
runGraph = run . runFresh 0 . runReader mempty


termAlgebra ::
  ( ConstructorName syntax
  , HasField fields Range
  , HasField fields Span
  , Foldable syntax
  , Member Fresh effs
  , Member (Reader (Graph (TaggedVertex tag))) effs
  )
  => tag
  -> TermF syntax (Record fields) (Eff effs (Graph (TaggedVertex tag)))
  -> Eff effs (Graph (TaggedVertex tag))
termAlgebra t (In ann syntax) = do
  i <- fresh
  parent <- ask
  let root = vertex (TaggedVertex i t (constructorName syntax) (getField ann) (getField ann))
  subGraph <- foldl' (\acc x -> overlay <$> acc <*> local (const root) x) (pure mempty) syntax
  pure (parent `connect` root `overlay` subGraph)

style :: (IsString string, Monoid string) => String -> (tag -> [Attribute string]) -> Style (TaggedVertex tag) string
style name tagAttributes = (defaultStyle (fromString . show . vertexId))
  { graphName = fromString (quote name)
  , vertexAttributes = vertexAttributes }
  where quote a = "\"" <> a <> "\""
        vertexAttributes TaggedVertex{..} = "label" := fromString vertexTermName : tagAttributes vertexTag

termStyle :: (IsString string, Monoid string) => String -> Style (TaggedVertex ()) string
termStyle name = style name (const [])

diffStyle :: (IsString string, Monoid string) => String -> Style (TaggedVertex DiffTag) string
diffStyle name = style name diffTagAttributes
  where diffTagAttributes Deleted  = ["color" := "red"]
        diffTagAttributes Inserted = ["color" := "green"]
        diffTagAttributes Replaced = ["color" := "orange", "style" := "dashed"]
        diffTagAttributes _        = []

data TaggedVertex tag
  = TaggedVertex
  { vertexId :: Int
  , vertexTag :: tag
  , vertexTermName :: String
  , vertexRange :: Range
  , vertexSpan :: Span
  } deriving (Eq, Ord, Show)

instance ToJSON (TaggedVertex ()) where
  toJSON TaggedVertex{..}
    = object $ [ "id"    .= T.pack (show vertexId)
               , "term"  .= vertexTermName ]
               <> toJSONFields vertexRange
               <> toJSONFields vertexSpan
  -- toEncoding TaggedVertex{..}
  --   = pairs (  "id"    .= T.pack (show vertexId)
  --           <> "name"  .= vertexTermName
  --           <> toJSONFields vertexRange )

instance JSONVertex (TaggedVertex ()) where
  jsonVertexId = T.pack . show . vertexId

data DiffTag = Deleted | Inserted | Replaced | Merged
  deriving (Eq, Ord, Show)


class ToTreeGraph vertex t | t -> vertex where
  toTreeGraph :: (Member Fresh effs, Member (Reader (Graph vertex)) effs) => t (Eff effs (Graph vertex)) -> Eff effs (Graph vertex)

instance (ConstructorName syntax, Foldable syntax, HasField fields Range, HasField fields Span) => ToTreeGraph (TaggedVertex ()) (TermF syntax (Record fields)) where
  toTreeGraph = termAlgebra ()

instance (ConstructorName syntax, Foldable syntax) => ToTreeGraph (TaggedVertex DiffTag) (DiffF syntax ann1 ann2) where
  toTreeGraph = undefined
  -- toTreeGraph d = case d of
  --   Merge t               -> termAlgebra Merged t
  --   Patch (Delete  t1)    ->          termAlgebra Deleted t1
  --   Patch (Insert     t2) ->                                     termAlgebra Inserted t2
  --   Patch (Replace t1 t2) -> do
  --     i <- fresh
  --     parent <- ask
  --     let replace = vertex (TaggedVertex i Replaced "Replacement")
  --     graph <- local (const replace) (overlay <$> termAlgebra Deleted t1 <*> termAlgebra Inserted t2)
  --     pure (parent `connect` replace `overlay` graph)
