{-# LANGUAGE MonoLocalBinds #-}
module Rendering.DOT
( renderDOTDiff
, renderDOTTerm
) where

import Algebra.Graph
import Algebra.Graph.Export.Dot
import Analysis.ConstructorName
import Data.Blob
import qualified Data.ByteString.Char8 as B
import Data.Diff
import Data.Foldable
import Data.Functor.Foldable hiding (fold)
import qualified Data.IntMap as IntMap
import Data.Patch
import Data.Semigroup
import Data.Term

renderDOTDiff :: (ConstructorName syntax, Foldable syntax, Functor syntax) => BlobPair -> Diff syntax ann1 ann2 -> B.ByteString
renderDOTDiff blobs diff = renderGraph (defaultStyleViaShow { graphName = B.pack (quote (pathKeyForBlobPair blobs)) }) (cata diffAlgebra diff 0 [])
  where quote a = "\"" <> a <> "\""

renderDOTTerm :: (ConstructorName syntax, Foldable syntax, Functor syntax) => Blob -> Term syntax ann -> B.ByteString
renderDOTTerm Blob{..} term = renderGraph (defaultStyleViaShow { graphName = B.pack blobPath }) (cata termAlgebra term 0 [])

diffAlgebra :: (ConstructorName syntax, Foldable syntax) => DiffF syntax ann1 ann2 (Int -> [Attribute B.ByteString] -> State) -> Int -> [Attribute B.ByteString] -> State
diffAlgebra d i as = case d of
  Merge t               -> termAlgebra t i as
  Patch (Delete  t1)    -> termAlgebra t1 i ("color" := "red"   : as)
  Patch (Insert     t2) -> termAlgebra t2 i ("color" := "green" : as)
  Patch (Replace t1 t2) -> let r1 =  termAlgebra t1 i                                             ("color" := "red"   : as)
                           in  r1 <> termAlgebra t2 (succ (maximum (i : toList (stateGraph r1)))) ("color" := "green" : as)

termAlgebra :: (ConstructorName syntax, Foldable syntax) => TermF syntax ann (Int -> [Attribute B.ByteString] -> State) -> Int -> [Attribute B.ByteString] -> State
termAlgebra t i as = State [succ i] g vattrs
  where (_, g, vattrs) = foldr combine (succ i, vertex (succ i), IntMap.singleton (succ i) ("label" := unConstructorLabel (constructorLabel t) : as)) (toList t)
        combine f (prev, g, attrs) = let State roots g' attrs' = f prev as in (maximum (prev : toList g'), foldr (overlay . connect (vertex (succ i)) . vertex) g' roots `overlay` g, attrs <> attrs')


data State = State { stateRoots :: [Int], stateGraph :: Graph Int, stateVertexAttributes :: IntMap.IntMap [Attribute B.ByteString] }

instance Semigroup State where
  State r1 g1 v1 <> State r2 g2 v2 = State (r1 <> r2) (g1 `overlay` g2) (v1 <> v2)


renderGraph :: Style Int B.ByteString -> State -> B.ByteString
renderGraph style State{..} = export (style { vertexAttributes = flip (IntMap.findWithDefault []) stateVertexAttributes }) stateGraph
