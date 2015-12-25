module Unified (unified, substring) where

import Diff
import Patch
import Syntax
import Term
import Range
import Source hiding ((++))
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.List hiding (foldl)
import qualified OrderedMap as Map
import Rainbow

unified :: Diff a Info -> Source Char -> Source Char -> IO ByteString
unified diff before after = do
  renderer <- byteStringMakerFromEnvironment
  return . mconcat . chunksToByteStrings renderer . fst $ iter g mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    g (Annotated (_, info) syntax) = annotationAndSyntaxToChunks after info syntax
    annotationAndSyntaxToChunks source (Info range _) (Leaf _) = (pure . chunk . toList $ slice range source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Indexed i) = (unifiedRange range i source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Fixed f) = (unifiedRange range f source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Keyed k) = (unifiedRange range (sort $ snd <$> Map.toList k) source, Just range)

    unifiedPatch :: Patch (Term a Info) -> [Chunk String]
    unifiedPatch patch = (fore red . bold <$> beforeChunk) <> (fore green . bold <$> afterChunk) where
      beforeChunk = maybe [] (change "-" . unifiedTerm before) $ Patch.before patch
      afterChunk = maybe [] (change "+" . unifiedTerm after) $ Patch.after patch

    unifiedTerm :: Source Char -> Term a Info -> [Chunk String]
    unifiedTerm source term = fst $ cata (annotationAndSyntaxToChunks source) term

    unifiedRange :: Range -> [([Chunk String], Maybe Range)] -> Source Char -> [Chunk String]
    unifiedRange range children source = out <> (pure . chunk . toList $ slice Range { start = previous, end = end range } source) where
      (out, previous) = foldl' accumulateContext ([], start range) children
      accumulateContext (out, previous) (child, Just range) = (mconcat [ out, pure . chunk . toList $ slice Range { start = previous, end = start range } source, child ], end range)
      accumulateContext (out, previous) (child, _) = (out <> child, previous)

range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

change :: String -> [Chunk String] -> [Chunk String]
change bound content = [ chunk "{", chunk bound ] ++ content ++ [ chunk bound, chunk "}" ]
