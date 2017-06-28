{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
module TOCSpec where

import Data.Aeson
import Category as C
import Data.Blob
import Data.Functor.Both
import Data.Functor.Listable
import Data.Record
import Data.Source
import Data.Text.Listable
import Data.These
import Diff
import Info
import Interpreter
import Language
import Patch
import Prologue hiding (fst, snd, readFile)
import Renderer
import Renderer.TOC
import RWS
import Semantic
import Semantic.Task
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

    let diffSize = max 1 . sum . fmap (const 1)
    let lastValue a = fromMaybe (extract a) (getLast (foldMap (Last . Just) a))
    prop "includes all nodes with a constant Just function" $
      \ diff -> let diff' = (unListableDiff diff :: Diff (Syntax ()) ()) in entryPayload <$> tableOfContentsBy (const (Just ())) diff' `shouldBe` replicate (diffSize diff') ()

    prop "produces an unchanged entry for identity diffs" $
      \ term -> let term' = (unListableF term :: Term (Syntax ()) (Record '[Category])) in tableOfContentsBy (Just . headF) (diffTerms (pure term')) `shouldBe` [Unchanged (lastValue term')]

    prop "produces inserted/deleted/replaced entries for relevant nodes within patches" $
      \ patch -> let patch' = (unListableF <$> patch :: Patch (Term (Syntax ()) Int)) in tableOfContentsBy (Just . headF) (pure patch') `shouldBe` these (pure . Deleted) (pure . Inserted) ((<>) `on` pure . Replaced) (unPatch (lastValue <$> patch'))

    prop "produces changed entries for relevant nodes containing irrelevant patches" $
      \ diff -> let diff' = fmap (1 <$) <$> mapAnnotations (const (0 :: Int)) (wrap (pure 0 :< Indexed [unListableDiff diff :: Diff (Syntax ()) Int])) in
        tableOfContentsBy (\ (n :< _) -> if n == 0 then Just n else Nothing) diff' `shouldBe`
        if Prologue.null diff' then [Unchanged 0]
                               else replicate (length diff') (Changed 0)

  describe "diffTOC" $ do
    it "blank if there are no methods" $
      diffTOC blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe`
        [ JSONSummary "Method" "self.foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
        , JSONSummary "Method" "bar" (sourceSpanBetween (4, 1) (6, 4)) "modified"
        , JSONSummary "Method" "baz" (sourceSpanBetween (4, 1) (5, 4)) "removed" ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "javascript/duplicate-parent.A.js" "javascript/duplicate-parent.B.js")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe`
        [ JSONSummary "Function" "myFunction" (sourceSpanBetween (1, 1) (6, 2)) "modified" ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "javascript/erroneous-duplicate-method.A.js" "javascript/erroneous-duplicate-method.B.js")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe`
        [ JSONSummary "Function" "performHealthCheck" (sourceSpanBetween (8, 1) (29, 2)) "modified" ]

    it "summarizes Go methods with receivers with special formatting" $ do
      sourceBlobs <- blobsForPaths (both "go/method-with-receiver.A.go" "go/method-with-receiver.B.go")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe`
        [ JSONSummary "Method" "(*apiClient) CheckAuth" (sourceSpanBetween (3,1) (3,101)) "added" ]

    it "summarizes Ruby methods that start with two identifiers" $ do
      sourceBlobs <- blobsForPaths (both "ruby/method-starts-with-two-identifiers.A.rb" "ruby/method-starts-with-two-identifiers.B.rb")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe`
        [ JSONSummary "Method" "foo" (sourceSpanBetween (1, 1) (4, 4)) "modified" ]

    it "handles unicode characters in file" $ do
      sourceBlobs <- blobsForPaths (both "ruby/unicode.A.rb" "ruby/unicode.B.rb")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe`
        [ JSONSummary "Method" "foo" (sourceSpanBetween (6, 1) (7, 4)) "added" ]

    it "properly slices source blob that starts with a newline and has multi-byte chars" $ do
      sourceBlobs <- blobsForPaths (both "javascript/starts-with-newline.js" "javascript/starts-with-newline.js")
      Just diff <- runTask (diffBlobPair IdentityDiffRenderer sourceBlobs)
      diffTOC diff `shouldBe` []

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
        diffTOC (diffTerms (pure term)) `shouldBe` []

  describe "JSONSummary" $ do
    it "encodes modified summaries to JSON" $ do
      let summary = JSONSummary "Method" "foo" (sourceSpanBetween (1, 1) (4, 4)) "modified"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[4,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"modified\"}"

    it "encodes added summaries to JSON" $ do
      let summary = JSONSummary "Method" "self.foo" (sourceSpanBetween (1, 1) (2, 4)) "added"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"}"

  describe "diff with ToCDiffRenderer" $ do
    it "produces JSON output" $ do
      blobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.B.rb")
      output <- runTask (diffBlobPair ToCDiffRenderer blobs)
      toS output `shouldBe` ("{\"changes\":{\"test/fixtures/toc/ruby/methods.A.rb -> test/fixtures/toc/ruby/methods.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"},{\"span\":{\"start\":[4,1],\"end\":[6,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"},{\"span\":{\"start\":[4,1],\"end\":[5,4]},\"category\":\"Method\",\"term\":\"baz\",\"changeType\":\"removed\"}]},\"errors\":{}}\n" :: ByteString)

    it "produces JSON output if there are parse errors" $ do
      blobs <- blobsForPaths (both "ruby/methods.A.rb" "ruby/methods.X.rb")
      output <- runTask (diffBlobPair ToCDiffRenderer blobs)
      toS output `shouldBe` ("{\"changes\":{\"test/fixtures/toc/ruby/methods.A.rb -> test/fixtures/toc/ruby/methods.X.rb\":[{\"span\":{\"start\":[4,1],\"end\":[5,4]},\"category\":\"Method\",\"term\":\"baz\",\"changeType\":\"removed\"}]},\"errors\":{\"test/fixtures/toc/ruby/methods.A.rb -> test/fixtures/toc/ruby/methods.X.rb\":[{\"span\":{\"start\":[1,1],\"end\":[3,1]},\"error\":\"def bar\\nen\\n\"}]}}\n" :: ByteString)

type Diff' = SyntaxDiff Text (Maybe Declaration ': DefaultFields)
type Term' = SyntaxTerm Text (Maybe Declaration ': DefaultFields)

numTocSummaries :: Diff' -> Int
numTocSummaries diff = length $ filter isValidSummary (diffTOC diff)

-- Return a diff where body is inserted in the expressions of a function. The function is present in both sides of the diff.
programWithChange :: Term' -> Diff'
programWithChange body = wrap (pure programInfo :< Indexed [ function' ])
  where
    function' = wrap (pure (Just (FunctionDeclaration "foo") :. functionInfo) :< S.Function name' [] [ inserting body ] )
    name' = wrap (pure (Nothing :. Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")

-- Return a diff where term is inserted in the program, below a function found on both sides of the diff.
programWithChangeOutsideFunction :: Term' -> Diff'
programWithChangeOutsideFunction term = wrap (pure programInfo :< Indexed [ function', term' ])
  where
    function' = wrap (pure (Just (FunctionDeclaration "foo") :. functionInfo) :< S.Function name' [] [] )
    name' = wrap (pure (Nothing :. Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf "foo")
    term' = inserting term

programWithInsert :: Text -> Term' -> Diff'
programWithInsert name body = programOf $ inserting (functionOf name body)

programWithDelete :: Text -> Term' -> Diff'
programWithDelete name body = programOf $ deleting (functionOf name body)

programWithReplace :: Text -> Term' -> Diff'
programWithReplace name body = programOf $ replacing (functionOf name body) (functionOf (name <> "2") body)

programOf :: Diff' -> Diff'
programOf diff = wrap (pure programInfo :< Indexed [ diff ])

functionOf :: Text -> Term' -> Term'
functionOf name body = cofree $ (Just (FunctionDeclaration name) :. functionInfo) :< S.Function name' [] [body]
  where
    name' = cofree $ (Nothing :. Range 0 0 :. C.Identifier :. sourceSpanBetween (0,0) (0,0) :. Nil) :< Leaf name

programInfo :: Record (Maybe Declaration ': DefaultFields)
programInfo = Nothing :. Range 0 0 :. C.Program :. sourceSpanBetween (0,0) (0,0) :. Nil

functionInfo :: Record DefaultFields
functionInfo = Range 0 0 :. C.Function :. sourceSpanBetween (0,0) (0,0) :. Nil

-- Filter tiers for terms that we consider "meaniningful" in TOC summaries.
isMeaningfulTerm :: ListableF (Term (Syntax leaf)) a -> Bool
isMeaningfulTerm a = case runCofree (unListableF a) of
  (_ :< S.Indexed _) -> False
  (_ :< S.Fixed _) -> False
  (_ :< S.Commented _ _) -> False
  (_ :< S.ParseError _) -> False
  _ -> True

-- Filter tiers for terms if the Syntax is a Method or a Function.
isMethodOrFunction :: HasField fields Category => ListableF (Term (Syntax leaf)) (Record fields) -> Bool
isMethodOrFunction a = case runCofree (unListableF a) of
  (_ :< S.Method{}) -> True
  (_ :< S.Function{}) -> True
  (a :< _) | getField a == C.Function -> True
  (a :< _) | getField a == C.Method -> True
  (a :< _) | getField a == C.SingletonMethod -> True
  _ -> False

blobsForPaths :: Both FilePath -> IO (Both Blob)
blobsForPaths = traverse (readFile . ("test/fixtures/toc/" <>))

sourceSpanBetween :: (Int, Int) -> (Int, Int) -> Span
sourceSpanBetween (s1, e1) (s2, e2) = Span (Pos s1 e1) (Pos s2 e2)

blankDiff :: Diff'
blankDiff = wrap (pure arrayInfo :< Indexed [ inserting (cofree $ literalInfo :< Leaf "\"a\"") ])
  where
    arrayInfo = Nothing :. Range 0 3 :. ArrayLiteral :. sourceSpanBetween (1, 1) (1, 5) :. Nil
    literalInfo = Nothing :. Range 1 2 :. StringLiteral :. sourceSpanBetween (1, 2) (1, 4) :. Nil

blankDiffBlobs :: Both Blob
blankDiffBlobs = both (Blob (fromText "[]") nullOid "a.js" (Just defaultPlainBlob) (Just TypeScript)) (Blob (fromText "[a]") nullOid "b.js" (Just defaultPlainBlob) (Just TypeScript))

instance Listable Text where
  tiers = unListableText `mapT` tiers
