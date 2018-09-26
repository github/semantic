{-# LANGUAGE DataKinds, MonoLocalBinds, TypeOperators #-}
module Rendering.TOC.Spec (spec) where

import Analysis.Declaration
import Data.Aeson hiding (defaultOptions)
import Data.Bifunctor
import Data.Bifunctor.Join
import Data.Diff
import Data.Functor.Classes
import Data.Hashable.Lifted
import Data.Patch
import Data.Range
import Data.Location
import Data.Span
import Data.Sum
import Data.Term
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Union
import Diffing.Algorithm
import Diffing.Interpreter
import Prelude
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import Rendering.TOC
import Semantic.Config

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "tableOfContentsBy" $ do
    prop "drops all nodes with the constant Nothing function" $
      \ diff -> tableOfContentsBy (const Nothing :: a -> Maybe ()) (diff :: Diff ListableSyntax () ()) `shouldBe` []

    prop "produces inserted/deleted/replaced entries for relevant nodes within patches" $
      \ p -> tableOfContentsBy (Just . termFAnnotation) (patch deleting inserting replacing p)
      `shouldBe`
      patch (fmap Deleted) (fmap Inserted) (\ as bs -> Replaced (head bs) : fmap Deleted (tail as) <> fmap Inserted (tail bs)) (bimap (foldMap pure) (foldMap pure) (p :: Patch (Term ListableSyntax Int) (Term ListableSyntax Int)))

    prop "produces changed entries for relevant nodes containing irrelevant patches" $
      \ diff -> do
        let diff' = merge (True, True) (inject [bimap (const False) (const False) (diff :: Diff ListableSyntax Bool Bool)])
        let toc = tableOfContentsBy (\ (n `In` _) -> if n then Just n else Nothing) diff'
        toc `shouldBe` [Changed True]

  describe "diffTOC" $ do
    it "blank if there are no methods" $
      diffTOC blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (both "ruby/toc/methods.A.rb" "ruby/toc/methods.B.rb")
      diff <- runTask $ diffWithParser rubyParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Method" "self.foo" (Span (Pos 1 1) (Pos 2 4)) "added"
        , TOCSummary "Method" "bar" (Span (Pos 4 1) (Pos 6 4)) "modified"
        , TOCSummary "Method" "baz" (Span (Pos 4 1) (Pos 5 4)) "removed"
        ]

    it "summarizes changed classes" $ do
      sourceBlobs <- blobsForPaths (both "ruby/toc/classes.A.rb" "ruby/toc/classes.B.rb")
      diff <- runTask $ diffWithParser rubyParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Class" "Baz" (Span (Pos 1 1) (Pos 2 4)) "removed"
        , TOCSummary "Class" "Foo" (Span (Pos 1 1) (Pos 3 4)) "modified"
        , TOCSummary "Class" "Bar" (Span (Pos 5 1) (Pos 6 4)) "added"
        ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (both "javascript/toc/duplicate-parent.A.js" "javascript/toc/duplicate-parent.B.js")
      diff <- runTask $ diffWithParser typescriptParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Function" "myFunction" (Span (Pos 1 1) (Pos 6 2)) "modified" ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (both "javascript/toc/erroneous-duplicate-method.A.js" "javascript/toc/erroneous-duplicate-method.B.js")
      diff <- runTask $ diffWithParser typescriptParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Function" "performHealthCheck" (Span (Pos 8 1) (Pos 29 2)) "modified" ]

    it "summarizes Go methods with receivers with special formatting" $ do
      sourceBlobs <- blobsForPaths (both "go/toc/method-with-receiver.A.go" "go/toc/method-with-receiver.B.go")
      diff <- runTask $ diffWithParser goParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Method" "(*apiClient) CheckAuth" (Span (Pos 3 1) (Pos 3 101)) "added" ]

    it "summarizes Ruby methods that start with two identifiers" $ do
      sourceBlobs <- blobsForPaths (both "ruby/toc/method-starts-with-two-identifiers.A.rb" "ruby/toc/method-starts-with-two-identifiers.B.rb")
      diff <- runTask $ diffWithParser rubyParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Method" "foo" (Span (Pos 1 1) (Pos 4 4)) "modified" ]

    it "handles unicode characters in file" $ do
      sourceBlobs <- blobsForPaths (both "ruby/toc/unicode.A.rb" "ruby/toc/unicode.B.rb")
      diff <- runTask $ diffWithParser rubyParser sourceBlobs
      diffTOC diff `shouldBe`
        [ TOCSummary "Method" "foo" (Span (Pos 6 1) (Pos 7 4)) "added" ]

    it "properly slices source blob that starts with a newline and has multi-byte chars" $ do
      sourceBlobs <- blobsForPaths (both "javascript/toc/starts-with-newline.js" "javascript/toc/starts-with-newline.js")
      diff <- runTaskWithOptions (defaultOptions { optionsLogLevel = Nothing }) $ diffWithParser typescriptParser sourceBlobs
      diffTOC diff `shouldBe` []

    prop "inserts of methods and functions are summarized" . forAll ((not . isMethodOrFunction . Prelude.snd) `filterT` tiers) $
      \(name, body) ->
        let diff = programWithInsert name body
        in numTocSummaries diff `shouldBe` 1

    prop "deletes of methods and functions are summarized" . forAll ((not . isMethodOrFunction . Prelude.snd) `filterT` tiers) $
      \(name, body) ->
        let diff = programWithDelete name body
        in numTocSummaries diff `shouldBe` 1

    prop "replacements of methods and functions are summarized" . forAll ((not . isMethodOrFunction . Prelude.snd) `filterT` tiers) $
      \(name, body) ->
        let diff = programWithReplace name body
        in numTocSummaries diff `shouldBe` 1

    prop "changes inside methods and functions are summarizied" . forAll (((&&) <$> not . isMethodOrFunction <*> isMeaningfulTerm) `filterT` tiers) $
      \body ->
        let diff = programWithChange body
        in numTocSummaries diff `shouldBe` 1

    prop "other changes don't summarize" . forAll ((not . isMethodOrFunction) `filterT` tiers) $
      \body ->
        let diff = programWithChangeOutsideFunction body
        in numTocSummaries diff `shouldBe` 0

  describe "TOCSummary" $ do
    it "encodes modified summaries to JSON" $ do
      let summary = TOCSummary "Method" "foo" (Span (Pos 1 1) (Pos 4 4)) "modified"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[4,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"modified\"}"

    it "encodes added summaries to JSON" $ do
      let summary = TOCSummary "Method" "self.foo" (Span (Pos 1 1) (Pos 2 4)) "added"
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"}"

  describe "diff with ToCDiffRenderer'" $ do
    it "produces JSON output" $ do
      blobs <- blobsForPaths (both "ruby/toc/methods.A.rb" "ruby/toc/methods.B.rb")
      output <- runTask (runDiff ToCDiffRenderer [blobs])
      runBuilder output `shouldBe` ("{\"changes\":{\"test/fixtures/ruby/toc/methods.A.rb -> test/fixtures/ruby/toc/methods.B.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"},{\"span\":{\"start\":[4,1],\"end\":[6,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"modified\"},{\"span\":{\"start\":[4,1],\"end\":[5,4]},\"category\":\"Method\",\"term\":\"baz\",\"changeType\":\"removed\"}]},\"errors\":{}}\n" :: ByteString)

    it "produces JSON output if there are parse errors" $ do
      blobs <- blobsForPaths (both "ruby/toc/methods.A.rb" "ruby/toc/methods.X.rb")
      output <- runTaskWithOptions (defaultOptions { optionsLogLevel = Nothing }) (runDiff ToCDiffRenderer [blobs])
      runBuilder output `shouldBe` ("{\"changes\":{\"test/fixtures/ruby/toc/methods.A.rb -> test/fixtures/ruby/toc/methods.X.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"bar\",\"changeType\":\"removed\"},{\"span\":{\"start\":[4,1],\"end\":[5,4]},\"category\":\"Method\",\"term\":\"baz\",\"changeType\":\"removed\"}]},\"errors\":{\"test/fixtures/ruby/toc/methods.A.rb -> test/fixtures/ruby/toc/methods.X.rb\":[{\"span\":{\"start\":[1,1],\"end\":[2,3]},\"error\":\"expected end of input nodes, but got ParseError\",\"language\":\"Ruby\"}]}}\n" :: ByteString)

    it "ignores anonymous functions" $ do
      blobs <- blobsForPaths (both "ruby/toc/lambda.A.rb" "ruby/toc/lambda.B.rb")
      output <- runTask (runDiff ToCDiffRenderer [blobs])
      runBuilder output `shouldBe` ("{\"changes\":{},\"errors\":{}}\n" :: ByteString)

    it "summarizes Markdown headings" $ do
      blobs <- blobsForPaths (both "markdown/toc/headings.A.md" "markdown/toc/headings.B.md")
      output <- runTask (runDiff ToCDiffRenderer [blobs])
      runBuilder output `shouldBe` ("{\"changes\":{\"test/fixtures/markdown/toc/headings.A.md -> test/fixtures/markdown/toc/headings.B.md\":[{\"span\":{\"start\":[1,1],\"end\":[3,16]},\"category\":\"Heading 1\",\"term\":\"Introduction\",\"changeType\":\"removed\"},{\"span\":{\"start\":[1,1],\"end\":[3,15]},\"category\":\"Heading 1\",\"term\":\"One\",\"changeType\":\"modified\"},{\"span\":{\"start\":[5,1],\"end\":[7,4]},\"category\":\"Heading 2\",\"term\":\"Two\",\"changeType\":\"modified\"},{\"span\":{\"start\":[9,1],\"end\":[11,10]},\"category\":\"Heading 3\",\"term\":\"This heading is new\",\"changeType\":\"added\"},{\"span\":{\"start\":[13,1],\"end\":[14,4]},\"category\":\"Heading 1\",\"term\":\"Final\",\"changeType\":\"added\"}]},\"errors\":{}}\n" :: ByteString)


type Diff' = Diff ListableSyntax (Maybe Declaration) (Maybe Declaration)
type Term' = Term ListableSyntax (Maybe Declaration)

numTocSummaries :: Diff' -> Int
numTocSummaries diff = length $ filter isValidSummary (diffTOC diff)

-- Return a diff where body is inserted in the expressions of a function. The function is present in both sides of the diff.
programWithChange :: Term' -> Diff'
programWithChange body = merge (Nothing, Nothing) (inject [ function' ])
  where
    function' = merge (Just (FunctionDeclaration "foo" mempty lowerBound Ruby), Just (FunctionDeclaration "foo" mempty lowerBound Ruby)) (inject (Declaration.Function [] name' [] (merge (Nothing, Nothing) (inject [ inserting body ]))))
    name' = merge (Nothing, Nothing) (inject (Syntax.Identifier (name "foo")))

-- Return a diff where term is inserted in the program, below a function found on both sides of the diff.
programWithChangeOutsideFunction :: Term' -> Diff'
programWithChangeOutsideFunction term = merge (Nothing, Nothing) (inject [ function', term' ])
  where
    function' = merge (Nothing, Nothing) (inject (Declaration.Function [] name' [] (merge (Nothing, Nothing) (inject []))))
    name' = merge (Nothing, Nothing) (inject (Syntax.Identifier (name "foo")))
    term' = inserting term

programWithInsert :: Text -> Term' -> Diff'
programWithInsert name body = programOf $ inserting (functionOf name body)

programWithDelete :: Text -> Term' -> Diff'
programWithDelete name body = programOf $ deleting (functionOf name body)

programWithReplace :: Text -> Term' -> Diff'
programWithReplace name body = programOf $ replacing (functionOf name body) (functionOf (name <> "2") body)

programOf :: Diff' -> Diff'
programOf diff = merge (Nothing, Nothing) (inject [ diff ])

functionOf :: Text -> Term' -> Term'
functionOf n body = termIn (Just (FunctionDeclaration n mempty lowerBound Unknown)) (inject (Declaration.Function [] name' [] (termIn Nothing (inject [body]))))
  where
    name' = termIn Nothing (inject (Syntax.Identifier (name n)))

-- Filter tiers for terms that we consider "meaniningful" in TOC summaries.
isMeaningfulTerm :: Term ListableSyntax a -> Bool
isMeaningfulTerm a
  | Just (_:_) <- project (termOut a) = False
  | Just []    <- project (termOut a) = False
  | otherwise                            = True

-- Filter tiers for terms if the Syntax is a Method or a Function.
isMethodOrFunction :: Term' -> Bool
isMethodOrFunction a
  | Just Declaration.Method{}   <- project (termOut a) = True
  | Just Declaration.Function{} <- project (termOut a) = True
  | any isJust (foldMap (:[]) a)                     = True
  | otherwise                                          = False

blobsForPaths :: Both FilePath -> IO BlobPair
blobsForPaths = readFilePair . fmap ("test/fixtures" </>)

blankDiff :: Diff'
blankDiff = merge (Nothing, Nothing) (inject [ inserting (termIn Nothing (inject (Syntax.Identifier (name "\"a\"")))) ])

-- Diff helpers
diffWithParser :: ( Eq1 syntax
                  , Show1 syntax
                  , Traversable syntax
                  , Diffable syntax
                  , HasDeclaration syntax
                  , Hashable1 syntax
                  , Member Distribute effs
                  , Member Task effs
                  )
               => Parser (Term syntax Location)
               -> BlobPair
               -> Eff effs (Diff syntax (Maybe Declaration) (Maybe Declaration))
diffWithParser parser blobs = distributeFor blobs (\ blob -> parse parser blob >>= decorate (declarationAlgebra blob)) >>= SpecHelpers.diff . runJoin
