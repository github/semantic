{-# LANGUAGE DataKinds, FlexibleContexts, MonoLocalBinds, OverloadedStrings, TupleSections, TypeOperators #-}
module Rendering.TOC.Spec (spec) where

import Analysis.TOCSummary
import Control.Effect.Parse
import Control.Effect.Reader
import Control.Monad.IO.Class
import Data.Aeson hiding (defaultOptions)
import Data.Bifunctor
import Data.Diff
import Data.Either (isRight)
import Data.Sum
import Data.Term
import Data.Text (Text)
import Diffing.Interpreter
import Prelude
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import Rendering.TOC
import Semantic.Api (diffSummaryBuilder, summarizeTerms, summarizeTermParsers)
import Serializing.Format as Format
import Source.Loc
import Source.Span
import qualified System.Path as Path
import           System.Path ((</>))

import SpecHelpers


spec :: Spec
spec = do
  describe "tableOfContentsBy" $ do
    prop "drops all nodes with the constant Nothing function" $
      \ diff -> tableOfContentsBy (const Nothing :: a -> Maybe ()) (diff :: Diff ListableSyntax () ()) `shouldBe` []

    prop "produces no entries for identity diffs" $
      \ term -> tableOfContentsBy (Just . termFAnnotation) (diffTerms term (term :: Term ListableSyntax ())) `shouldBe` []

    prop "produces inserted/deleted/replaced entries for relevant nodes within patches" $
      \ p -> tableOfContentsBy (Just . termFAnnotation) (edit deleting inserting comparing p)
      `shouldBe`
      edit (fmap (Deleted,)) (fmap (Inserted,)) (\ as bs -> (Replaced, head bs) : fmap (Deleted,) (tail as) <> fmap (Inserted,) (tail bs)) (bimap (foldMap pure) (foldMap pure) (p :: Edit (Term ListableSyntax Int) (Term ListableSyntax Int)))

    prop "produces changed entries for relevant nodes containing irrelevant patches" $
      \ diff -> do
        let diff' = merge (True, True) (inject [bimap (const False) (const False) (diff :: Diff ListableSyntax Bool Bool)])
        let toc = tableOfContentsBy (\ (n `In` _) -> if n then Just n else Nothing) diff'
        toc `shouldBe` if null (diffPatches diff') then []
                                                   else [(Changed, True)]

  describe "diffTOC" $ do
    it "blank if there are no methods" $
      diffTOC blankDiff `shouldBe` [ ]

    it "summarizes changed methods" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "ruby/toc/methods.A.rb") (Path.relFile "ruby/toc/methods.B.rb")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe`
        [ Right $ TOCSummary (Method (Just "self")) "self.foo" (Span (Pos 1 1) (Pos 2 4)) Inserted
        , Right $ TOCSummary (Method Nothing) "bar" (Span (Pos 4 1) (Pos 6 4)) Changed
        , Right $ TOCSummary (Method Nothing) "baz" (Span (Pos 4 1) (Pos 5 4)) Deleted
        ]

    it "dedupes changes in same parent method" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "javascript/toc/duplicate-parent.A.js") (Path.relFile "javascript/toc/duplicate-parent.B.js")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe`
        [ Right $ TOCSummary Function "myFunction" (Span (Pos 1 1) (Pos 6 2)) Changed ]

    it "dedupes similar methods" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "javascript/toc/erroneous-duplicate-method.A.js") (Path.relFile "javascript/toc/erroneous-duplicate-method.B.js")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe`
        [ Right $ TOCSummary Function "performHealthCheck" (Span (Pos 8 1) (Pos 29 2)) Replaced ]

    it "summarizes Go methods with receivers with special formatting" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "go/toc/method-with-receiver.A.go") (Path.relFile "go/toc/method-with-receiver.B.go")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe`
        [ Right $ TOCSummary (Method (Just "*apiClient")) "(*apiClient) CheckAuth" (Span (Pos 3 1) (Pos 3 101)) Inserted ]

    it "summarizes Ruby methods that start with two identifiers" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "ruby/toc/method-starts-with-two-identifiers.A.rb") (Path.relFile "ruby/toc/method-starts-with-two-identifiers.B.rb")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe`
        [ Right $ TOCSummary (Method Nothing) "foo" (Span (Pos 1 1) (Pos 4 4)) Changed ]

    it "handles unicode characters in file" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "ruby/toc/unicode.A.rb") (Path.relFile "ruby/toc/unicode.B.rb")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe`
        [ Right $ TOCSummary (Method Nothing) "foo" (Span (Pos 6 1) (Pos 7 4)) Inserted ]

    it "properly slices source blob that starts with a newline and has multi-byte chars" $ do
      sourceBlobs <- blobsForPaths (Path.relFile "javascript/toc/starts-with-newline.js") (Path.relFile "javascript/toc/starts-with-newline.js")
      diff <- runTaskOrDie $ summarize sourceBlobs
      diff `shouldBe` []

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

    prop "unchanged diffs aren’t summarized" $
      \term -> diffTOC (diffTerms term (term :: Term')) `shouldBe` []

  describe "TOCSummary" $ do
    it "encodes modified summaries to JSON" $ do
      let summary = TOCSummary (Method Nothing) "foo" (Span (Pos 1 1) (Pos 4 4)) Changed
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[4,4]},\"category\":\"Method\",\"term\":\"foo\",\"changeType\":\"modified\"}"

    it "encodes added summaries to JSON" $ do
      let summary = TOCSummary (Method Nothing) "self.foo" (Span (Pos 1 1) (Pos 2 4)) Inserted
      encode summary `shouldBe` "{\"span\":{\"start\":[1,1],\"end\":[2,4]},\"category\":\"Method\",\"term\":\"self.foo\",\"changeType\":\"added\"}"

  describe "diff with ToCDiffRenderer'" $ do
    it "produces JSON output" $ do
      blobs <- blobsForPaths (Path.relFile "ruby/toc/methods.A.rb") (Path.relFile "ruby/toc/methods.B.rb")
      output <- runTaskOrDie (runReader defaultLanguageModes (diffSummaryBuilder Format.JSON [blobs]))
      runBuilder output `shouldBe` ("{\"files\":[{\"path\":\"test/fixtures/ruby/toc/methods.A.rb -> test/fixtures/ruby/toc/methods.B.rb\",\"language\":\"Ruby\",\"changes\":[{\"category\":\"Method\",\"term\":\"self.foo\",\"span\":{\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":2,\"column\":4}},\"changeType\":\"ADDED\"},{\"category\":\"Method\",\"term\":\"bar\",\"span\":{\"start\":{\"line\":4,\"column\":1},\"end\":{\"line\":6,\"column\":4}},\"changeType\":\"MODIFIED\"},{\"category\":\"Method\",\"term\":\"baz\",\"span\":{\"start\":{\"line\":4,\"column\":1},\"end\":{\"line\":5,\"column\":4}},\"changeType\":\"REMOVED\"}]}]}\n" :: ByteString)

    it "produces JSON output if there are parse errors" $ do
      blobs <- blobsForPaths (Path.relFile "ruby/toc/methods.A.rb") (Path.relFile "ruby/toc/methods.X.rb")
      output <- runTaskOrDie (runReader defaultLanguageModes (diffSummaryBuilder Format.JSON [blobs]))
      runBuilder output `shouldBe` ("{\"files\":[{\"path\":\"test/fixtures/ruby/toc/methods.A.rb -> test/fixtures/ruby/toc/methods.X.rb\",\"language\":\"Ruby\",\"changes\":[{\"category\":\"Method\",\"term\":\"bar\",\"span\":{\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":2,\"column\":4}},\"changeType\":\"REMOVED\"},{\"category\":\"Method\",\"term\":\"baz\",\"span\":{\"start\":{\"line\":4,\"column\":1},\"end\":{\"line\":5,\"column\":4}},\"changeType\":\"REMOVED\"}],\"errors\":[{\"error\":\"expected end of input nodes, but got ParseError\",\"span\":{\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":2,\"column\":3}}}]}]}\n" :: ByteString)

    it "ignores anonymous functions" $ do
      blobs <- blobsForPaths (Path.relFile "ruby/toc/lambda.A.rb") (Path.relFile "ruby/toc/lambda.B.rb")
      output <- runTaskOrDie (runReader defaultLanguageModes (diffSummaryBuilder Format.JSON [blobs]))
      runBuilder output `shouldBe` ("{\"files\":[{\"path\":\"test/fixtures/ruby/toc/lambda.A.rb -> test/fixtures/ruby/toc/lambda.B.rb\",\"language\":\"Ruby\"}]}\n" :: ByteString)

    it "summarizes Markdown headings" $ do
      blobs <- blobsForPaths (Path.relFile "markdown/toc/headings.A.md") (Path.relFile "markdown/toc/headings.B.md")
      output <- runTaskOrDie (runReader defaultLanguageModes (diffSummaryBuilder Format.JSON [blobs]))
      runBuilder output `shouldBe` ("{\"files\":[{\"path\":\"test/fixtures/markdown/toc/headings.A.md -> test/fixtures/markdown/toc/headings.B.md\",\"language\":\"Markdown\",\"changes\":[{\"category\":\"Heading 1\",\"term\":\"Introduction\",\"span\":{\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":3,\"column\":16}},\"changeType\":\"REMOVED\"},{\"category\":\"Heading 2\",\"term\":\"Two\",\"span\":{\"start\":{\"line\":5,\"column\":1},\"end\":{\"line\":7,\"column\":4}},\"changeType\":\"MODIFIED\"},{\"category\":\"Heading 3\",\"term\":\"This heading is new\",\"span\":{\"start\":{\"line\":9,\"column\":1},\"end\":{\"line\":11,\"column\":10}},\"changeType\":\"ADDED\"},{\"category\":\"Heading 1\",\"term\":\"Final\",\"span\":{\"start\":{\"line\":13,\"column\":1},\"end\":{\"line\":14,\"column\":4}},\"changeType\":\"ADDED\"}]}]}\n" :: ByteString)


type Diff' = Diff ListableSyntax (Maybe Declaration) (Maybe Declaration)
type Term' = Term ListableSyntax (Maybe Declaration)

numTocSummaries :: Diff' -> Int
numTocSummaries diff = length $ filter isRight (diffTOC diff)

-- Return a diff where body is inserted in the expressions of a function. The function is present in both sides of the diff.
programWithChange :: Term' -> Diff'
programWithChange body = merge (Nothing, Nothing) (inject [ function' ])
  where
    function' = merge (Just (Declaration Function "foo" lowerBound Ruby), Just (Declaration Function "foo" lowerBound Ruby)) (inject (Declaration.Function [] name' [] (merge (Nothing, Nothing) (inject [ inserting body ]))))
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
programWithReplace name body = programOf $ comparing (functionOf name body) (functionOf (name <> "2") body)

programOf :: Diff' -> Diff'
programOf diff = merge (Nothing, Nothing) (inject [ diff ])

functionOf :: Text -> Term' -> Term'
functionOf n body = termIn (Just (Declaration Function n lowerBound Unknown)) (inject (Declaration.Function [] name' [] (termIn Nothing (inject [body]))))
  where
    name' = termIn Nothing (inject (Syntax.Identifier (name n)))

-- Filter tiers for terms that we consider "meaniningful" in TOC summaries.
isMeaningfulTerm :: Term ListableSyntax a -> Bool
isMeaningfulTerm a
  | Just (_:_) <- project (termOut a) = False
  | Just []    <- project (termOut a) = False
  | otherwise                         = True

-- Filter tiers for terms if the Syntax is a Method or a Function.
isMethodOrFunction :: Term' -> Bool
isMethodOrFunction a
  | Just Declaration.Method{}   <- project (termOut a) = True
  | Just Declaration.Function{} <- project (termOut a) = True
  | any isJust (foldMap (:[]) a)                       = True
  | otherwise                                          = False

blobsForPaths :: Path.RelFile -> Path.RelFile -> IO BlobPair
blobsForPaths p1 p2 = readFilePathPair (prefix p1) (prefix p2) where
  prefix = (Path.relDir "test/fixtures" </>)

blankDiff :: Diff'
blankDiff = merge (Nothing, Nothing) (inject [ inserting (termIn Nothing (inject (Syntax.Identifier (name "\"a\"")))) ])

-- Diff helpers
summarize
  :: (Member (Error SomeException) sig, Member Parse sig, Member Telemetry sig, Carrier sig m, MonadIO m)
  => BlobPair
  -> m [Either ErrorSummary TOCSummary]
summarize = parsePairWith (summarizeTermParsers defaultLanguageModes) summarizeTerms
