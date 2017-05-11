{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
module TOCSpec where

import Data.Aeson
import Category as C
import Data.Functor.Both
import Data.Functor.Listable
import Data.Maybe (fromJust)
import Data.Record
import Data.Text.Listable
import Diff
import Info
import Interpreter
import Patch
import Prologue hiding (fst, snd)
import Renderer
import Renderer.TOC
import RWS
import Semantic
import Source
import SpecHelpers
import Syntax as S
import Term
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Expectations.Pretty
import Test.Hspec.LeanCheck
import Test.LeanCheck

spec :: Spec
spec = parallel $ do
  describe "tableOfContentsBy" $ do
    prop "drops all nodes with the constant Nothing function" $
      \ diff -> tableOfContentsBy (const Nothing :: a -> Maybe ()) (unListableDiff diff :: Diff (Syntax ()) ()) `shouldBe` []

    let entryValue e = case e of { Unchanged a -> a; Changed a -> a ; Patched p -> afterOrBefore p }
    let diffSize = cata (succ . sum) . fmap (termSize . afterOrBefore)
    let lastValue a = fromMaybe (extract a) (getLast (foldMap (Last . Just) a))
    prop "includes all nodes with a constant Just function" $
      \ diff -> let diff' = (unListableDiff diff :: Diff (Syntax ()) ()) in entryValue <$> tableOfContentsBy (const (Just ())) diff' `shouldBe` replicate (diffSize diff') ()

    prop "produces an unchanged entry for identity diffs" $
      \ term -> let term' = (unListableF term :: Term (Syntax ()) (Record '[Category])) in tableOfContentsBy (Just . headF) (diffTerms term' term') `shouldBe` [Unchanged (lastValue term')]

  describe "diffTOC" $ do
    it "blank if there are no methods" $
      diffTOC blankDiffBlobs blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      Just diff <- diffBlobPair declarationDecorator sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.SingletonMethod "self.foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
        , JSONSummary $ Summarizable C.Method "bar" (sourceSpanBetween (4, 1) (6, 4)) "modified"
        , JSONSummary $ Summarizable C.Method "baz" (sourceSpanBetween (4, 1) (5, 4)) "removed" ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "javascript/duplicate-parent.A.js" "javascript/duplicate-parent.B.js")
      Just diff <- diffBlobPair declarationDecorator sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Function "myFunction" (sourceSpanBetween (1, 1) (6, 2)) "modified" ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "javascript/erroneous-duplicate-method.A.js" "javascript/erroneous-duplicate-method.B.js")
      Just diff <- diffBlobPair declarationDecorator sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Function "performHealthCheck" (sourceSpanBetween (8, 1) (29, 2)) "modified" ]

    it "summarizes Go methods with receivers with special formatting" $ do
      sourceBlobs <- blobsForPaths (both "go/method-with-receiver.A.go" "go/method-with-receiver.B.go")
      Just diff <- diffBlobPair declarationDecorator sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Method "(*apiClient) CheckAuth" (sourceSpanBetween (3,1) (3,101)) "added" ]

    it "summarizes Ruby methods that start with two identifiers" $ do
      sourceBlobs <- blobsForPaths (both "ruby/method-starts-with-two-identifiers.A.rb" "ruby/method-starts-with-two-identifiers.B.rb")
      Just diff <- diffBlobPair declarationDecorator sourceBlobs
      diffTOC sourceBlobs diff `shouldBe`
        [ JSONSummary $ Summarizable C.Method "foo" (sourceSpanBetween (1, 1) (4, 4)) "modified" ]

    it "handles unicode characters in file" $ do
      sourceBlobs <- blobsForPaths (both "ruby/unicode.A.rb" "ruby/unicode.B.rb")
      Just diff <- diffBlobPair declarationDecorator sourceBlobs
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
      let summary = JSONSummary $ Summarizable C.Method "foo" (sourceSpanBetween (1, 1) (4, 4)) "modified"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[4,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"modified\"}"

    it "encodes Summarizable to JSON" $ do
      let summary = JSONSummary $ Summarizable C.SingletonMethod "self.foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"}"

  describe "diff with ToCRenderer" $ do
    it "produces JSON output" $ do
      blobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      output <- diffBlobPairs declarationDecorator ToCRenderer [blobs]
      output `shouldBe` "{\"changes\":{\"ruby/methods.A.rb -> ruby/methods.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"},{\"span\":{\"start\":[4,1],\"end\":[6,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"},{\"span\":{\"start\":[4,1],\"end\":[5,4]},\"category\":\"Method\",\"term\":\"baz\",\"changeType\":\"removed\"}]},\"errors\":{}}\n"

    it "produces JSON output if there are parse errors" $ do
      blobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.X.rb")
      output <- diffBlobPairs declarationDecorator ToCRenderer [blobs]
      output `shouldBe` "{\"changes\":{},\"errors\":{\"ruby/methods.A.rb -> ruby/methods.X.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,1]},\"error\":\"def bar\\nen\\n\"}]}}\n"

type Diff' = SyntaxDiff Text DefaultFields
type Term' = SyntaxTerm Text DefaultFields

numTocSummaries :: Diff' -> Int
numTocSummaries diff = Prologue.length $ filter isValidSummary (diffTOC blankDiffBlobs diff)

-- Return a diff where body is inserted in the expressions of a function. The function is present in both sides of the diff.
programWithChange :: Term' -> Diff'
programWithChange body = wrap (pure programInfo :< Indexed [ function' ])
  where
    function' = wrap (pure functionInfo :< S.Function name' [] [ inserting body ] )
    name' = wrap (pure (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")

-- Return a diff where term is inserted in the program, below a function found on both sides of the diff.
programWithChangeOutsideFunction :: Term' -> Diff'
programWithChangeOutsideFunction term = wrap (pure programInfo :< Indexed [ function', term' ])
  where
    function' = wrap (pure functionInfo :< S.Function name' [] [] )
    name' = wrap (pure (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")
    term' = inserting term

programWithInsert :: Text -> Term' -> Diff'
programWithInsert name body = programOf $ Insert (functionOf name body)

programWithDelete :: Text -> Term' -> Diff'
programWithDelete name body = programOf $ Delete (functionOf name body)

programWithReplace :: Text -> Term' -> Diff'
programWithReplace name body = programOf $ Replace (functionOf name body) (functionOf (name <> "2") body)

programOf :: Patch Term' -> Diff'
programOf patch = wrap (pure programInfo :< Indexed [ pure patch ])

functionOf :: Text -> Term' -> Term'
functionOf name body = cofree $ functionInfo :< S.Function name' [] [body]
  where
    name' = cofree $ (Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf name

programInfo :: Record DefaultFields
programInfo = Range 0 0 :. C.Program :. sourceSpanBetween (0,0) (0,0) :. Nil

functionInfo :: Record DefaultFields
functionInfo = Range 0 0 :. C.Function :. sourceSpanBetween (0,0) (0,0) :. Nil

-- Filter tiers for terms that we consider "meaniningful" in TOC summaries.
isMeaningfulTerm :: ListableF (Term (Syntax leaf)) (Record DefaultFields) -> Bool
isMeaningfulTerm a = case runCofree (unListableF a) of
  (_ :< S.Indexed _) -> False
  (_ :< S.Fixed _) -> False
  (_ :< S.Commented _ _) -> False
  (_ :< S.ParseError _) -> False
  _ -> True

-- Filter tiers for terms if the Syntax is a Method or a Function.
isMethodOrFunction :: ListableF (Term (Syntax leaf)) (Record DefaultFields) -> Bool
isMethodOrFunction a = case runCofree (unListableF a) of
  (_ :< S.Method{}) -> True
  (_ :< S.Function{}) -> True
  (a :< _) | getField a == C.Function -> True
  (a :< _) | getField a == C.Method -> True
  (a :< _) | getField a == C.SingletonMethod -> True
  _ -> False

blobsForPaths :: Both FilePath -> IO (Both SourceBlob)
blobsForPaths paths = do
  sources <- traverse (readFileToUnicode . ("test/fixtures/toc/" <>)) paths
  pure $ SourceBlob <$> sources <*> pure mempty <*> paths <*> pure (Just Source.defaultPlainBlob)

sourceSpanBetween :: (Int, Int) -> (Int, Int) -> SourceSpan
sourceSpanBetween (s1, e1) (s2, e2) = SourceSpan (SourcePos s1 e1) (SourcePos s2 e2)

blankDiff :: Diff (Syntax Text) (Record '[Category, Range, SourceSpan])
blankDiff = wrap (pure arrayInfo :< Indexed [ inserting (cofree $ literalInfo :< Leaf "\"a\"") ])
  where
    arrayInfo = ArrayLiteral :. Range 0 3 :. sourceSpanBetween (1, 1) (1, 5) :. Nil
    literalInfo = StringLiteral :. Range 1 2 :. sourceSpanBetween (1, 2) (1, 4) :. Nil

blankDiffBlobs :: Both SourceBlob
blankDiffBlobs = both (SourceBlob (fromText "[]") nullOid "a.js" (Just defaultPlainBlob)) (SourceBlob (fromText "[a]") nullOid "b.js" (Just defaultPlainBlob))

instance Listable Text where
  tiers = unListableText `mapT` tiers
