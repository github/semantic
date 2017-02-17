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
import Syntax as S
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "tocSummaries" $ do
    it "blank if there are no methods" $
      diffTOC blankDiffBlobs blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Method "foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
        , JSONSummary $ InSummarizable C.Method "bar" (sourceSpanBetween (4, 1) (6, 4))
        , JSONSummary $ Summarizable C.Method "baz" (sourceSpanBetween (4, 1) (5, 4)) "removed" ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "javascript/dupParent.A.js" "javascript/dupParent.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ InSummarizable C.Function "myFunction" (sourceSpanBetween (1, 1) (6, 2)) ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "javascript/erroneousDupMethod.A.js" "javascript/erroneousDupMethod.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Function "performHealthCheck" (sourceSpanBetween (8, 1) (29, 2)) "modified" ]

    it "summarizes Go methods with receivers with special formatting" $ do
      sourceBlobs <- blobsForPaths (both "go/method-with-receiver.A.go" "go/method-with-receiver.B.go")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Method "(*apiClient) CheckAuth" (sourceSpanBetween (3,1) (3,101)) "added" ]

    it "handles unicode characters in file" $ do
      sourceBlobs <- blobsForPaths (both "ruby/unicode.A.rb" "ruby/unicode.B.rb")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Method "foo" (sourceSpanBetween (6, 1) (7, 4)) "added" ]

    prop "inserts of methods and functions are summarized" $
      \name body ->
        let diff = programWithInsert name (unListableF body)
        in numTocSummaries diff `shouldBe` 1

    prop "deletes of methods and functions are summarized" $
      \name body ->
        let diff = programWithDelete name (unListableF body)
        in numTocSummaries diff `shouldBe` 1

    prop "replacements of methods and functions are summarized" $
      \name body ->
        let diff = programWithReplace name (unListableF body)
        in numTocSummaries diff `shouldBe` 1

    prop "changes inside methods and functions are summarizied" . forAll (isMeaningfulTerm `filterT` tiers) $
      \body ->
        let diff = programWithChange (unListableF body)
        in numTocSummaries diff `shouldBe` 1

    prop "other changes don't summarize" . forAll ((not . isMethodOrFunction) `filterT` tiers) $
      \body ->
        let diff = programWithChangeOutsideFunction (unListableF body)
        in numTocSummaries diff `shouldBe` 0

    prop "equal terms produce identity diffs" $
      \a -> let term = defaultFeatureVectorDecorator (Info.category . headF) (unListableF a :: Term') in
        diffTOC blankDiffBlobs (diffTerms wrap (==) diffCost term term) `shouldBe` []

type Diff' = SyntaxDiff String '[Range, Category, SourceSpan]
type Term' = SyntaxTerm String '[Range, Category, SourceSpan]

numTocSummaries :: Diff' -> Int
numTocSummaries diff = Prologue.length $ filter (not . isErrorSummary) (diffTOC blankDiffBlobs diff)

-- Return a diff where body is inserted in the expressions of a function. The function is present in both sides of the diff.
programWithChange :: Term' -> Diff'
programWithChange body = free $ Free (pure programInfo :< Indexed [ function' ])
  where
    function' = free $ Free (pure functionInfo :< S.Function name' [] Nothing [ free $ Pure (Insert body) ] )
    name' = free $ Free (pure (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")

-- Return a diff where term is inserted in the program, below a function found on both sides of the diff.
programWithChangeOutsideFunction :: Term' -> Diff'
programWithChangeOutsideFunction term = free $ Free (pure programInfo :< Indexed [ function', term' ])
  where
    function' = free $ Free (pure functionInfo :< S.Function name' [] Nothing [] )
    name' = free $ Free (pure (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")
    term' = free $ Pure (Insert term)

programWithInsert :: String -> Term' -> Diff'
programWithInsert name body = programOf $ Insert (functionOf name body)

programWithDelete :: String -> Term' -> Diff'
programWithDelete name body = programOf $ Delete (functionOf name body)

programWithReplace :: String -> Term' -> Diff'
programWithReplace name body = programOf $ Replace (functionOf name body) (functionOf (name <> "2") body)

programOf :: Patch Term' -> Diff'
programOf patch = free $ Free (pure programInfo :< Indexed [ free $ Pure patch ])

functionOf :: String -> Term' -> Term'
functionOf name body = cofree $ functionInfo :< S.Function name' [] Nothing [body]
  where
    name' = cofree $ (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf name

programInfo :: Record '[Range, Category, SourceSpan]
programInfo = Range 0 0 :. C.Program :. sourceSpanBetween (0,0) (0,0) :. Nil

functionInfo :: Record '[Range, Category, SourceSpan]
functionInfo = Range 0 0 :. C.Function :. sourceSpanBetween (0,0) (0,0) :. Nil

-- Filter tiers for terms that we consider "meaniningful" in TOC summaries.
isMeaningfulTerm :: ListableF (Term (Syntax leaf)) (Record '[Range, Category, SourceSpan]) -> Bool
isMeaningfulTerm a = case runCofree (unListableF a) of
  (_ :< S.Indexed _) -> False
  (_ :< S.Fixed _) -> False
  (_ :< S.Commented _ _) -> False
  (_ :< S.ParseError _) -> False
  _ -> True

-- Filter tiers for terms if the Syntax is a Method or a Function.
isMethodOrFunction :: ListableF (Term (Syntax leaf)) (Record '[Range, Category, SourceSpan]) -> Bool
isMethodOrFunction a = case runCofree (unListableF a) of
  (_ :< S.Method{}) -> True
  (_ :< S.Function{}) -> True
  _ -> False

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

blankDiff :: Diff (Syntax Text) (Record '[Category, Range, SourceSpan])
blankDiff = free $ Free (pure arrayInfo :< Indexed [ free $ Pure (Insert (cofree $ literalInfo :< Leaf "\"a\"")) ])
  where
    arrayInfo = ArrayLiteral :. Range 0 3 :. sourceSpanBetween (1, 1) (1, 5) :. Nil
    literalInfo = StringLiteral :. Range 1 2 :. sourceSpanBetween (1, 2) (1, 4) :. Nil

blankDiffBlobs :: Both SourceBlob
blankDiffBlobs = both (SourceBlob (fromText "[]") nullOid "a.js" (Just defaultPlainBlob)) (SourceBlob (fromText "[a]") nullOid "b.js" (Just defaultPlainBlob))

unListableDiff :: Functor f => ListableF (Free (TermF f (ListableF (Join (,)) annotation))) (Patch (ListableF (Term f) annotation)) -> Diff f annotation
unListableDiff diff = hoistFree (first unListableF) $ fmap unListableF <$> unListableF diff
