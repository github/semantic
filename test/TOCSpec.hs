{-# LANGUAGE DataKinds #-}
module TOCSpec where

import Category
import Data.Functor.Both
import Data.RandomWalkSimilarity
import Data.Record
import Data.These
import Diff
import Diffing
import Info
import Interpreter
import Parse
import Prologue hiding (fst, snd)
import Renderer.TOC
import Source
import Syntax
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "tocSummaries" $ do
    it "blank if there are no methods" $
      diffTOC blobs blankDiff `shouldBe` [ ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "test/corpus/toc/javascript/dupParent.A.js" "test/corpus/toc/javascript/dupParent.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe` [ JSONSummary $ InSummarizable Category.Function "myFunction" (sourceSpanBetween (1, 1) (6, 2)) ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "test/corpus/toc/javascript/test.A.js" "test/corpus/toc/javascript/test.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe` [ JSONSummary $ Summarizable Category.Function "performHealthCheck" (sourceSpanBetween (8, 1) (29, 2)) "modified" ]

testDiff :: Both SourceBlob -> IO (Diff (Syntax Text) (Record '[Cost, Range, Category, SourceSpan]))
testDiff sourceBlobs = do
  terms <- traverse (fmap (defaultFeatureVectorDecorator getLabel) . parser) sourceBlobs
  pure $! stripDiff (diffTerms' terms sourceBlobs)
  where
    parser = parserWithCost (path . fst $ sourceBlobs)
    diffTerms' terms blobs = case runBothWith areNullOids blobs of
      (True, False) -> pure $ Insert (snd terms)
      (False, True) -> pure $ Delete (fst terms)
      (_, _) -> runBothWith (diffTerms construct compareCategoryEq diffCostWithCachedTermCosts) terms
    areNullOids a b = (hasNullOid a, hasNullOid b)
    hasNullOid blob = oid blob == nullOid || Source.null (source blob)
    construct (info :< syntax) = free (Free ((setCost <$> info <*> sumCost syntax) :< syntax))
    sumCost = fmap getSum . foldMap (fmap Sum . getCost)
    getCost diff = case runFree diff of
      Free (info :< _) -> cost <$> info
      Pure patch -> uncurry both (fromThese 0 0 (unPatch (cost . extract <$> patch)))

blobsForPaths :: Both FilePath -> IO (Both SourceBlob)
blobsForPaths paths = do
  sources <- sequence $ readAndTranscodeFile <$> paths
  pure $ SourceBlob <$> sources <*> pure mempty <*> paths <*> pure (Just Source.defaultPlainBlob)


sourceSpanBetween :: (Int, Int) -> (Int, Int) -> SourceSpan
sourceSpanBetween (s1, e1) (s2, e2) = SourceSpan (SourcePos s1 e1) (SourcePos s2 e2)

arrayInfo :: Record '[Category, Range, SourceSpan]
arrayInfo = ArrayLiteral :. Range 0 3 :. sourceSpanBetween (1, 1) (1, 5) :. Nil

literalInfo :: Record '[Category, Range, SourceSpan]
literalInfo = StringLiteral :. Range 1 2 :. sourceSpanBetween (1, 2) (1, 4) :. Nil

blankDiff :: Diff (Syntax Text) (Record '[Category, Range, SourceSpan])
blankDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "\"a\"")) ])

blobs :: Both SourceBlob
blobs = both (SourceBlob (fromText "[]") nullOid "a.js" (Just defaultPlainBlob)) (SourceBlob (fromText "[a]") nullOid "b.js" (Just defaultPlainBlob))
