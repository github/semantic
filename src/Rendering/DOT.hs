module Rendering.DOT where

import Analysis.ConstructorName
import Control.Applicative
import Data.Blob
import qualified Data.ByteString.Char8 as B
import Data.Diff
import Data.Foldable
import Data.Functor.Both (Both)
import Data.Functor.Foldable hiding (fold)
import Data.Semigroup
import Data.Term

renderDOTDiff :: Both Blob -> Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff _ _ = ""

renderDOTTerm :: (ConstructorName syntax, Foldable syntax, Functor syntax) => Blob -> Term syntax ann -> B.ByteString
renderDOTTerm Blob{..} term = renderGraph (snd (cata graphAlgebra term)) { graphName = Just (B.pack blobPath) }


graphAlgebra :: (ConstructorName syntax, Foldable syntax) => TermF syntax ann (Int, Graph) -> (Int, Graph)
graphAlgebra t = (i, Graph
  Nothing
  (Node i (unConstructorLabel (constructorLabel t)) : (g >>= graphNodes . snd))
  (map (Edge i . fst) g <> (g >>= graphEdges . snd)))
  where g = toList t
        i = 0


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
