{-# LANGUAGE DataKinds #-}
module TOCSpec where

import Data.Aeson
import Category as C
import Command
import Data.Functor.Both
import Data.Functor.Listable
import Data.RandomWalkSimilarity
import Data.Record
import Data.String
import Diff
import Info
import Interpreter
import Patch
import Prologue hiding (fst, snd)
import Renderer
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
  describe "diffTOC" $ do
    it "blank if there are no methods" $
      diffTOC blankDiffBlobs blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.SingletonMethod "self.foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
        , JSONSummary $ InSummarizable C.Method "bar" (sourceSpanBetween (4, 1) (6, 4))
        , JSONSummary $ Summarizable C.Method "baz" (sourceSpanBetween (4, 1) (5, 4)) "removed" ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "javascript/duplicate-parent.A.js" "javascript/duplicate-parent.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ InSummarizable C.Function "myFunction" (sourceSpanBetween (1, 1) (6, 2)) ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "javascript/erroneous-duplicate-method.A.js" "javascript/erroneous-duplicate-method.B.js")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Function "performHealthCheck" (sourceSpanBetween (8, 1) (29, 2)) "modified" ]

    it "summarizes Go methods with receivers with special formatting" $ do
      sourceBlobs <- blobsForPaths (both "go/method-with-receiver.A.go" "go/method-with-receiver.B.go")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Method "(*apiClient) CheckAuth" (sourceSpanBetween (3,1) (3,101)) "added" ]

    it "summarizes Ruby methods that start with two identifiers" $ do
      sourceBlobs <- blobsForPaths (both "ruby/method-starts-with-two-identifiers.A.rb" "ruby/method-starts-with-two-identifiers.B.rb")
      diff <- testDiff sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ InSummarizable C.Method "foo" (sourceSpanBetween (1, 1) (4, 4)) ]

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
        diffTOC blankDiffBlobs (diffTerms term term) `shouldBe` []

  describe "JSONSummary" $ do
    it "encodes InSummarizable to JSON" $ do
      let summary = JSONSummary $ InSummarizable C.Method "foo" (sourceSpanBetween (1, 1) (4, 4))
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[4,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"modified\"}"

    it "encodes Summarizable to JSON" $ do
      let summary = JSONSummary $ Summarizable C.SingletonMethod "self.foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"}"

  describe "diffFiles" $ do
    it "encodes to final JSON" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      output <- diffOutput sourceBlobs
      output `shouldBe` "{\"changes\":{\"ruby/methods.A.rb -> ruby/methods.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"},{\"span\":{\"start\":[4,1],\"end\":[6,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"},{\"span\":{\"start\":[4,1],\"end\":[5,4]},\"category\":\"Method\",\"term\":\"baz\",\"changeType\":\"removed\"}]},\"errors\":{}}"

    it "encodes to final JSON if there are parse errors" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.X.rb")
      output <- diffOutput sourceBlobs
      output `shouldBe` "{\"changes\":{},\"errors\":{\"ruby/methods.A.rb -> ruby/methods.X.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,1]},\"error\":\"def bar\\nen\\n\"}]}}"

type Diff' = SyntaxDiff String '[Range, Category, SourceSpan]
type Term' = SyntaxTerm String '[Range, Category, SourceSpan]

diffOutput :: Both SourceBlob -> IO ByteString
diffOutput sourceBlobs = do
  let parser = parserForFilepath (path (fst sourceBlobs))
  diff <- diffFiles parser sourceBlobs
  pure $ concatOutputs [toc sourceBlobs diff]

numTocSummaries :: Diff' -> Int
numTocSummaries diff = Prologue.length $ filter (not . isErrorSummary) (diffTOC blankDiffBlobs diff)

-- Return a diff where body is inserted in the expressions of a function. The function is present in both sides of the diff.
programWithChange :: Term' -> Diff'
programWithChange body = free $ Free (pure programInfo :< Indexed [ function' ])
  where
    function' = free $ Free (pure functionInfo :< S.Function name' [] [ free $ Pure (Insert body) ] )
    name' = free $ Free (pure (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")

-- Return a diff where term is inserted in the program, below a function found on both sides of the diff.
programWithChangeOutsideFunction :: Term' -> Diff'
programWithChangeOutsideFunction term = free $ Free (pure programInfo :< Indexed [ function', term' ])
  where
    function' = free $ Free (pure functionInfo :< S.Function name' [] [] )
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
functionOf name body = cofree $ functionInfo :< S.Function name' [] [body]
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
  (a :< _) | getField a == C.Function -> True
  (a :< _) | getField a == C.Method -> True
  (a :< _) | getField a == C.SingletonMethod -> True
  _ -> False

testDiff :: Both SourceBlob -> IO (Diff (Syntax Text) (Record '[Range, Category, SourceSpan]))
testDiff sourceBlobs = diffFiles parser sourceBlobs
  where
    parser = parserForFilepath (path . fst $ sourceBlobs)

blobsForPaths :: Both FilePath -> IO (Both SourceBlob)
blobsForPaths paths = do
  sources <- traverse (readAndTranscodeFile . ("test/fixtures/toc/" <>)) paths
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
