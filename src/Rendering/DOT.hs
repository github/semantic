{-# LANGUAGE MonoLocalBinds #-}
module Rendering.DOT
( renderDOTDiff
, renderDOTTerm
) where

import Analysis.ConstructorName
import Control.Applicative
import Data.Blob
import qualified Data.ByteString.Char8 as B
import Data.Diff
import Data.Foldable
import Data.Function (on)
import Data.Functor.Both (Both, runBothWith)
import Data.Functor.Foldable hiding (fold)
import qualified Data.Map as Map
import Data.Patch
import Data.Semigroup
import Data.Term

renderDOTDiff :: (ConstructorName syntax, Foldable syntax, Functor syntax) => Both Blob -> Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff blobs diff = renderGraph (snd (cata diffAlgebra diff 0)) { graphName = Just (runBothWith (fmap B.pack . (join `on` blobPath)) blobs) }
  where join = (++) . (" -> " ++)

renderDOTTerm :: (ConstructorName syntax, Foldable syntax, Functor syntax) => Blob -> Term syntax ann -> B.ByteString
renderDOTTerm Blob{..} term = renderGraph (snd (cata termAlgebra term 0)) { graphName = Just (B.pack blobPath) }

diffAlgebra :: (ConstructorName syntax, Foldable syntax) => DiffF syntax ann1 ann2 (Int -> (Int, Graph)) -> Int -> (Int, Graph)
diffAlgebra d i = case d of
  Merge t               -> termAlgebra t i
  Patch (Delete  t1)    -> termAlgebra t1 i `modifyHeadNode` setColour "red"
  Patch (Insert     t2) -> termAlgebra t2 i `modifyHeadNode` setColour "green"
  Patch (Replace t1 t2) -> let (_, g1) = termAlgebra t1 i `modifyHeadNode` setColour "red"
                               (_, g2) = termAlgebra t2 i `modifyHeadNode` setColour "green"
                           in  (succ i, g1 <> g2)
  where modifyHeadNode (i, g) f | n:ns <- graphNodes g = (i, g { graphNodes = f n : ns })
                                | otherwise            = (i, g)
        setColour c n = n { nodeAttributes = Map.insert "color" c (nodeAttributes n) }

termAlgebra :: (ConstructorName syntax, Foldable syntax) => TermF syntax ann (Int -> (Int, Graph)) -> Int -> (Int, Graph)
termAlgebra t i = (succ i, Graph
  Nothing
  (Node (succ i) (Map.singleton "label" (unConstructorLabel (constructorLabel t))) : graphNodes g)
  (map (Edge (succ i)) is <> graphEdges g))
  where (_, is, g) = foldr combine (succ i, [], mempty) (toList t)
        combine f (i, is, gs) = let (i', g) = f i in (maximum (i : map nodeID (graphNodes g)), i' : is, g <> gs)


data Graph = Graph { graphName :: Maybe B.ByteString, graphNodes :: [Node], graphEdges :: [Edge] }
  deriving (Eq, Ord, Show)

data Node = Node { nodeID :: Int, nodeAttributes :: Map.Map B.ByteString B.ByteString }
  deriving (Eq, Ord, Show)

data Edge = Edge { edgeFrom :: Int, edgeTo :: Int }
  deriving (Eq, Ord, Show)


renderGraph :: Graph -> B.ByteString
renderGraph Graph{..} = "digraph " <> maybe "" quote graphName <> " {" <> foldr ((<>) . renderNode) "" graphNodes <> foldr ((<>) . renderEdge) "" graphEdges <> "}"
  where quote a = "\"" <> a <> "\""

renderNode :: Node -> B.ByteString
renderNode Node{..} = B.pack (show nodeID) <> " [ " <> foldr (\ (key, value) rest -> key <> " = \"" <> value <> "\" " <> rest) "" (Map.toList nodeAttributes) <> "];"

renderEdge :: Edge -> B.ByteString
renderEdge Edge{..} = B.pack (show edgeFrom) <> " -> " <> B.pack (show edgeTo) <> ";"


instance Semigroup Graph where
  Graph n1 ns1 es1 <> Graph n2 ns2 es2 = Graph (n1 <|> n2) (ns1 <> ns2) (es1 <> es2)

instance Monoid Graph where
  mempty = Graph Nothing [] []
  mappend = (<>)
