module Rendering.DOT where

import Control.Applicative
import Data.Blob
import qualified Data.ByteString.Char8 as B
import Data.Diff
import Data.Functor.Both
import Data.Semigroup
import Data.Term

renderDOTDiff :: Both Blob -> Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff _ _ = ""

renderDOTTerm :: Blob -> Term syntax ann -> B.ByteString
renderDOTTerm _ _ = ""


renderGraph :: Graph -> B.ByteString
renderGraph Graph{..} = "digraph " <> maybe "" quote graphName <> " {" <> foldr ((<>) . renderNode) "" graphNodes <> foldr ((<>) . renderEdge) "" graphEdges <> "}"
  where quote a = "\"" <> a <> "\""

renderNode :: Node -> B.ByteString
renderNode Node{..} = B.pack (show nodeID) <> " [ label = " <> nodeLabel <> " ]"

renderEdge :: Edge -> B.ByteString
renderEdge Edge{..} = B.pack (show edgeFrom) <> " -> " <> B.pack (show edgeTo) <> ";"

data Graph = Graph { graphName :: Maybe B.ByteString, graphNodes :: [Node], graphEdges :: [Edge] }
  deriving (Eq, Ord, Show)

data Node = Node { nodeID :: Int, nodeLabel :: B.ByteString }
  deriving (Eq, Ord, Show)

data Edge = Edge { edgeFrom :: Int, edgeTo :: Int }
  deriving (Eq, Ord, Show)

instance Semigroup Graph where
  Graph n1 ns1 es1 <> Graph n2 ns2 es2 = Graph (n1 <|> n2) (ns1 <> ns2) (es1 <> es2)

instance Monoid Graph where
  mempty = Graph Nothing [] []
  mappend = (<>)
