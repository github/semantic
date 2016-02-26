module Renderer.Unified (unified, substring) where

import Diff
import Patch
import Syntax
import Term
import Range
import Renderer
import Source hiding ((++))
import Control.Arrow
import Control.Monad.Free
import Control.Comonad.Cofree
import Data.List hiding (foldl)
import qualified Data.OrderedMap as Map
import Rainbow

-- | Render a diff with the unified format.
unified :: Renderer a [Chunk String]
unified diff (beforeBlob, afterBlob) = fst $ iter g mapped where
    mapped = fmap (unifiedPatch &&& range) diff
    toChunk = chunk . toList
    g (Annotated (_, info) syntax) = annotationAndSyntaxToChunks (source afterBlob) info syntax
    -- | Render an annotation and syntax into a list of chunks.
    annotationAndSyntaxToChunks source (Info range _) (Leaf _) = ([ toChunk $ slice range source ], Just range)
    annotationAndSyntaxToChunks source (Info range _) (Indexed i) = (unifiedRange range i source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Fixed f) = (unifiedRange range f source, Just range)
    annotationAndSyntaxToChunks source (Info range _) (Keyed k) = (unifiedRange range (snd <$> Map.toList k) source, Just range)

    -- | Render a Patch into a list of chunks.
    unifiedPatch :: Patch (Term a Info) -> [Chunk String]
    unifiedPatch patch = (fore red . bold <$> beforeChunk) <> (fore green . bold <$> afterChunk) where
      before = source beforeBlob
      after = source afterBlob
      beforeChunk = maybe [] (change "-" . unifiedTerm before) $ Patch.before patch
      afterChunk = maybe [] (change "+" . unifiedTerm after) $ Patch.after patch

    -- | Render the contents of a Term as a series of chunks.
    unifiedTerm :: Source Char -> Term a Info -> [Chunk String]
    unifiedTerm source term = fst $ cata (annotationAndSyntaxToChunks source) term

    -- | Given a range and a list of pairs of chunks and a range, render the
    -- | entire range from the source as a single list of chunks.
    unifiedRange :: Range -> [([Chunk String], Maybe Range)] -> Source Char -> [Chunk String]
    unifiedRange range children source = out <> [ toChunk $ slice Range { start = previous, end = end range } source ]
      where
        (out, previous) = foldl' accumulateContext ([], start range) children
        accumulateContext (out, previous) (child, Just range) = (out <> [ toChunk $ slice Range { start = previous, end = start range } source ] <> child, end range)
        accumulateContext (out, previous) (child, _) = (out <> child, previous)

-- | Return the range of the after side of the patch, or Nothing if it's not a replacement.
range :: Patch (Term a Info) -> Maybe Range
range patch = range . extract <$> after patch where
  extract (annotation :< _) = annotation
  range (Info range _) = range

-- | Add chunks to the beginning and end of the list with curly braces and the given string.
change :: String -> [Chunk String] -> [Chunk String]
change bound content = [ chunk "{", chunk bound ] ++ content ++ [ chunk bound, chunk "}" ]
