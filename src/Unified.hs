module Unified (unified, substring) where

import Diff
import Patch
import Syntax
import Term
import Range
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.List hiding (foldl)
import qualified Data.Text as T
import qualified OrderedMap as Map
import Rainbow

unified :: Diff a Info -> T.Text -> T.Text -> IO ByteString
unified diff before after = do
  renderer <- byteStringMakerFromEnvironment
  return . mconcat . chunksToByteStrings renderer . fst $ iter g mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    g (Annotated (_, info) syntax) = annotationAndSyntaxToChunks after info syntax

    annotationAndSyntaxToChunks source (Info range _) (Leaf _) = (pure . chunk $ substring range source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Indexed i) = (unifiedRange range i source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Fixed f) = (unifiedRange range f source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Keyed k) = (unifiedRange range (sort $ snd <$> Map.toList k) source, Just range)

    unifiedPatch :: Patch (Term a Info) -> [Chunk T.Text]
    unifiedPatch patch = (fore red . bold <$> beforeChunk) <> (fore green . bold <$> afterChunk) where
      beforeChunk = maybe [] (change "-" . unifiedTerm before) $ Patch.before patch
      afterChunk = maybe [] (change "+" . unifiedTerm after) $ Patch.after patch

    unifiedTerm source term = fst $ cata (annotationAndSyntaxToChunks source) term

    unifiedRange :: Range -> [([Chunk T.Text], Maybe Range)] -> T.Text -> [Chunk T.Text]
    unifiedRange range children source = out <> (pure . chunk $ substring Range { start = previous, end = end range } source) where
      (out, previous) = foldl accumulateContext ([], start range) children
      accumulateContext (out, previous) (child, Just range) = (mconcat [ out, pure . chunk $ substring Range { start = previous, end = start range } source, child ], end range)
      accumulateContext (out, previous) (child, _) = (out <> child, previous)

range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

change :: T.Text -> [Chunk T.Text] -> [Chunk T.Text]
change bound content = [ chunk "{", chunk bound ] ++ content ++ [ chunk bound, chunk "}" ]
