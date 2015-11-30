module Unified (unified, substring) where

import Diff
import Patch
import Syntax
import Term
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.List hiding (foldl)
import qualified Data.Map as Map
import Rainbow

unified :: Diff a Info -> String -> String -> IO ByteString
unified diff before after = do
  renderer <- byteStringMakerFromEnvironment
  return . mconcat . chunksToByteStrings renderer . fst $ iter g mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    g (Annotated (_, info) syntax) = annotationAndSyntaxToChunks info syntax
    annotationAndSyntaxToChunks (Info range _) (Leaf _) = (pure . chunk $ substring range after, Just range)
    annotationAndSyntaxToChunks (Info range _) (Indexed i) = (unifiedRange range i after, Just range)
    annotationAndSyntaxToChunks (Info range _) (Fixed f) = (unifiedRange range f after, Just range)
    annotationAndSyntaxToChunks (Info range _) (Keyed k) = (unifiedRange range (sort $ snd <$> Map.toList k) after, Just range)

    unifiedPatch :: Patch (Term a Info) -> [Chunk String]
    unifiedPatch patch = (fore red . bold <$> beforeChunk) <> (fore green . bold <$> afterChunk) where
      beforeChunk = maybe [] (change "-" . unifiedTerm before) $ Patch.before patch
      afterChunk = maybe [] (change "+" . unifiedTerm after) $ Patch.after patch

    unifiedTerm :: String -> Term a Info -> [Chunk String]
    unifiedTerm source term = fst $ cata annotationAndSyntaxToChunks term

    unifiedRange :: Range -> [([Chunk String], Maybe Range)] -> String -> [Chunk String]
    unifiedRange range children source = out <> (pure . chunk $ substring Range { start = previous, end = end range } after) where
      (out, previous) = foldl accumulateContext ([], start range) children
      accumulateContext (out, previous) (child, Just range) = (mconcat [ out, pure . chunk $ substring Range { start = previous, end = start range } source, child ], end range)
      accumulateContext (out, previous) (child, _) = (out <> child, previous)

substring :: Range -> String -> String
substring range = take (end range - start range) . drop (start range)

range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

change :: String -> [Chunk String] -> [Chunk String]
change bound content = [ chunk "{", chunk bound ] ++ content ++ [ chunk bound, chunk "}" ]

instance Ord Range where
  a <= b = start a <= start b
