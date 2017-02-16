{-# LANGUAGE DataKinds #-}
module TOCSpec where

import Category as C
import Data.Functor.Both
import Data.Functor.Listable
import Data.RandomWalkSimilarity
import Data.Record
import Data.These
import Data.String
import Diff
import Diffing
import Info
import Interpreter
import Parse
import Patch
import Prologue hiding (fst, snd)
import Renderer.TOC
import Source
import Syntax
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "tocSummaries" $ do
    it "blank if there are no methods" $
      diffTOC blobs blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe` [ JSONSummary $ Summarizable C.Method "foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
                                          , JSONSummary $ InSummarizable C.Method "bar" (sourceSpanBetween (4, 1) (6, 4))
                                          , JSONSummary $ Summarizable C.Method "baz" (sourceSpanBetween (4, 1) (5, 4)) "removed" ]
    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "javascript/dupParent.A.js" "javascript/dupParent.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe` [ JSONSummary $ InSummarizable C.Function "myFunction" (sourceSpanBetween (1, 1) (6, 2)) ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "javascript/erroneousDupMethod.A.js" "javascript/erroneousDupMethod.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe` [ JSONSummary $ Summarizable C.Function "performHealthCheck" (sourceSpanBetween (8, 1) (29, 2)) "modified" ]

    prop "only methods and functions are summarized" $
      \iden body ->
        let
          -- diff = functionOf (unListableDiff body) (unListableDiff <$> params) (unListableDiff <$> ty) [unListableDiff iden]
          diff = functionOf' (unListableDiff iden) (unListableDiff body)
          tocSummaries = filter (not . isErrorSummary) (diffTOC blobs diff)
          numPatches = sum (1 <$ diff)
        in
          trace ("toc:" <> show tocSummaries <> " diff: " <> show diff :: String)
            ((isJust (beforeTerm diff) && isJust (afterTerm diff) ==> Prologue.length tocSummaries == max 1 numPatches) `shouldBe` True)

    prop "equal terms produce identity diffs" $
      \a -> let term = defaultFeatureVectorDecorator (Info.category . headF) (unListableF a :: SyntaxTerm String '[Category, Range, SourceSpan]) in
        diffTOC blobs (diffTerms wrap (==) diffCost term term) `shouldBe` []

type Diff' = SyntaxDiff String '[Range, Category, SourceSpan]

programOf :: Diff' -> Diff'
programOf child = wrap (pure (Range 0 0 :. C.Program :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Indexed [ child ])

functionOf :: Diff' -> [Diff'] -> Maybe Diff' -> [Diff'] -> Diff'
functionOf iden params ty body = wrap (pure (Range 0 0 :. C.Function :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Syntax.Function iden params ty body)

functionOf' :: Diff' -> Diff' -> Diff'
functionOf' iden body = wrap (pure (Range 0 0 :. C.Function :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Syntax.Function iden [] Nothing [body])

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
  sources <- sequence $ readAndTranscodeFile . ("test/corpus/toc/" <>) <$> paths
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

unListableDiff :: Functor f => ListableF (Free (TermF f (ListableF (Join (,)) annotation))) (Patch (ListableF (Term f) annotation)) -> Diff f annotation
unListableDiff diff = hoistFree (first unListableF) $ fmap unListableF <$> unListableF diff
