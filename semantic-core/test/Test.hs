{-# LANGUAGE OverloadedStrings, TypeApplications, TypeOperators #-}
module Main (main) where

import qualified Text.Trifecta as Trifecta

import           Hedgehog hiding (Var)
import           Test.Tasty
import           Test.Tasty.Hedgehog
import           Test.Tasty.HUnit

import           Analysis.File
import           Core.Core
import           Core.Pretty
import           Core.Parser as Parse
import qualified Core.Eval as Eval
import           Core.Name
import qualified Generators as Gen
import           Source.Span
import           Syntax.Term

-- * Helpers

true, false :: Term Core Name
true  = bool True
false = bool False

parseEither :: Trifecta.Parser a -> String -> Either String a
parseEither p = Trifecta.foldResult (Left . show . Trifecta._errDoc) Right . Trifecta.parseString (p <* Trifecta.eof) mempty

-- * Parser roundtripping properties. Note that parsing and prettyprinting is generally
-- not a roundtrip, because the parser inserts 'Ann' nodes itself.

prop_roundtrips :: Gen (Term Core Name) -> Property
prop_roundtrips gen = property $ do
  input <- forAll gen
  tripping input showCore (parseEither (Parse.core <* Trifecta.eof))

parserProps :: TestTree
parserProps = testGroup "Parsing: roundtripping"
  [ testProperty "literals" $ prop_roundtrips Gen.literal
  , testProperty "if/then/else" . prop_roundtrips . Gen.ifthenelse $ Gen.variable
  , testProperty "lambda" . prop_roundtrips $ Gen.lambda Gen.literal
  , testProperty "function application" . prop_roundtrips $ Gen.apply Gen.variable
  , testProperty "expressions" . prop_roundtrips $ Gen.expr
  ]

-- * Parser specs

parsesInto :: String -> Term Core Name -> Assertion
parsesInto str res = case parseEither Parse.core str of
  Right x -> x @?= res
  Left m  -> assertFailure m

assert_booleans_parse :: Assertion
assert_booleans_parse = do
  parseEither Parse.core "#true"  @?= Right true
  parseEither Parse.core "#false" @?= Right false

a, f, g, h :: Term Core Name
(a, f, g, h) = (pure "a", pure "f", pure "g", pure "h")

assert_ifthen_parse :: Assertion
assert_ifthen_parse = "if #true then #true else #false" `parsesInto` (if' true true false)

assert_application_parse :: Assertion
assert_application_parse = "f g" `parsesInto` (f $$ g)

assert_application_left_associative :: Assertion
assert_application_left_associative = "f g h" `parsesInto` (f $$ g $$ h)

assert_push_left_associative :: Assertion
assert_push_left_associative = "f.g.h" `parsesInto` (f ... "g" ... "h")

assert_ascii_lambda_parse :: Assertion
assert_ascii_lambda_parse = "\\a -> a" `parsesInto` lam (named' "a") a

assert_unicode_lambda_parse :: Assertion
assert_unicode_lambda_parse = "λa → a" `parsesInto` lam (named' "a") a

assert_quoted_name_parse :: Assertion
assert_quoted_name_parse = "#{(NilClass)}" `parsesInto` pure "(NilClass)"

parserSpecs :: TestTree
parserSpecs = testGroup "Parsing: simple specs"
  [ testCase "true/false" assert_booleans_parse
  , testCase "if/then/else" assert_ifthen_parse
  , testCase "function application" assert_application_parse
  , testCase "application is left-associative" assert_application_left_associative
  , testCase "dotted push is left-associative" assert_push_left_associative
  , testCase "lambda with ASCII syntax" assert_ascii_lambda_parse
  , testCase "lambda with unicode syntax" assert_unicode_lambda_parse
  , testCase "quoted names" assert_quoted_name_parse
  ]

assert_roundtrips :: File (Term Core Name) -> Assertion
assert_roundtrips (File _ _ core) = case parseEither Parse.core (showCore core) of
  Right v -> v @?= core
  Left  e -> assertFailure e

parserExamples :: TestTree
parserExamples = testGroup "Parsing: Eval.hs examples"
  [ testCase "prog1" (assert_roundtrips Eval.prog1)
  , testCase "prog2" (assert_roundtrips Eval.prog2)
  , testCase "prog3" (assert_roundtrips Eval.prog3)
  , testCase "prog4" (assert_roundtrips Eval.prog4)
  , testCase "prog6.1" (assert_roundtrips (head Eval.prog6))
  , testCase "prog6.2" (assert_roundtrips (last Eval.prog6))
  , testCase "ruby" (assert_roundtrips (stripAnnotations @Span <$> Eval.ruby))
  ]

tests :: TestTree
tests = testGroup "semantic-core"
  [ parserSpecs
  , parserExamples
  , parserProps
  ]

main :: IO ()
main = defaultMain tests
