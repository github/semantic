{-# LANGUAGE MonoLocalBinds #-}
module Rendering.DOT
( renderDOTDiff
, renderDOTTerm
) where

import Analysis.ConstructorName
import Control.Applicative
import Data.Bifunctor.Join (Join(..))
import Data.Blob
import qualified Data.ByteString.Char8 as B
import Data.Diff
import Data.Foldable
import Data.Functor.Foldable hiding (fold)
import qualified Data.Map as Map
import Data.Patch
import Data.Semigroup
import Data.Term
import Data.These (These, mergeThese)

renderDOTDiff :: (ConstructorName syntax, Foldable syntax, Functor syntax) => Join These Blob -> Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff blobs diff = renderGraph (snd (cata diffAlgebra diff 0)) { graphName = Just (B.pack (mergeThese combine (runJoin (blobPath <$> blobs)))) }
  where combine p1 p2 = p1 <> " -> " <> p2

renderDOTTerm :: (ConstructorName syntax, Foldable syntax, Functor syntax) => Blob -> Term syntax ann -> B.ByteString
renderDOTTerm Blob{..} term = renderGraph (snd (cata termAlgebra term 0)) { graphName = Just (B.pack blobPath) }

diffAlgebra :: (ConstructorName syntax, Foldable syntax) => DiffF syntax ann1 ann2 (Int -> ([Int], Graph)) -> Int -> ([Int], Graph)
diffAlgebra d i = case d of
  Merge t               -> termAlgebra t i
  Patch (Delete  t1)    -> termAlgebra t1 i `modifyHeadNode` setColour "red"
  Patch (Insert     t2) -> termAlgebra t2 i `modifyHeadNode` setColour "green"
  Patch (Replace t1 t2) -> let (i', g1) =  termAlgebra t1 i                         `modifyHeadNode` setColour "red"
                           in  (i', g1) <> termAlgebra t2 (succ (maximum (i : i'))) `modifyHeadNode` setColour "green"
  where modifyHeadNode (i, g) f | n:ns <- graphNodes g = (i, g { graphNodes = f n : ns })
                                | otherwise            = (i, g)
        setColour c n = n { nodeAttributes = Map.insert "color" c (nodeAttributes n) }

termAlgebra :: (ConstructorName syntax, Foldable syntax) => TermF syntax ann (Int -> ([Int], Graph)) -> Int -> ([Int], Graph)
termAlgebra t i = ([succ i], Graph
  Nothing
  (Node (succ i) (Map.singleton "label" (unConstructorLabel (constructorLabel t))) : graphNodes g)
  (concatMap (map (Edge (succ i))) is <> graphEdges g))
  where (_, is, g) = foldr combine (succ i, [], mempty) (toList t)
        combine f (i, is, gs) = let (i', g) = f i in (maximum (i : map nodeID (graphNodes g)), i' : is, g <> gs)


data Graph = Graph { graphName :: Maybe B.ByteString, graphNodes :: [Node], graphEdges :: [Edge] }
  deriving (Eq, Ord, Show)

data Node = Node { nodeID :: Int, nodeAttributes :: Map.Map B.ByteString B.ByteString }
  deriving (Eq, Ord, Show)

data Edge = Edge { edgeFrom :: Int, edgeTo :: Int }
  deriving (Eq, Ord, Show)


renderGraph :: Graph -> B.ByteString
renderGraph Graph{..} = "digraph " <> maybe "" quote graphName <> " {\n" <> foldr ((<>) . renderNode) "" graphNodes <> foldr ((<>) . renderEdge) "" graphEdges <> "}"
  where quote a = "\"" <> a <> "\""

renderNode :: Node -> B.ByteString
renderNode Node{..} = "\t" <> B.pack (show nodeID) <> " [ " <> foldr (\ (key, value) rest -> key <> " = \"" <> value <> "\" " <> rest) "" (Map.toList nodeAttributes) <> "];\n"

renderEdge :: Edge -> B.ByteString
renderEdge Edge{..} = "\t" <> B.pack (show edgeFrom) <> " -> " <> B.pack (show edgeTo) <> ";\n"


instance Semigroup Graph where
  Graph n1 ns1 es1 <> Graph n2 ns2 es2 = Graph (n1 <|> n2) (ns1 <> ns2) (es1 <> es2)

instance Monoid Graph where
  mempty = Graph Nothing [] []
  mappend = (<>)
