{-# LANGUAGE TypeOperators #-}

module Rewriting.Go.Spec (spec) where

import           Control.Rewriting
import           Data.List
import           Data.Sum
import qualified Data.Syntax.Literal as Lit
import qualified Data.Syntax.Statement as Stmt
import           Data.Text (Text)
import           SpecHelpers

-- This gets the Text contents of all integers
integerMatcher :: (Lit.Integer :< fs) => Rewrite (Term (Sum fs) ann) Text
integerMatcher = enter Lit.integerContent

-- This matches all for-loops with its index variable new variable bound to 0,
-- e.g. `for i := 0; i < 10; i++`
loopMatcher :: ( Stmt.For :< fs
               , Stmt.Assignment :< fs
               , Lit.Integer :< fs)
            => Rule (Term (Sum fs) ann)
loopMatcher = target <* go where
  go = enter Stmt.forBefore
       >>> enter Stmt.assignmentValue
       >>> enter Lit.integerContent
       >>> ensure (== "0")


spec :: Spec
spec = describe "recursively" $ do
  it "extracts integers" $ do
    parsed <- parseFileQuiet goParser "test/fixtures/go/matching/integers.go"
    let matched = recursively integerMatcher parsed
    sort matched `shouldBe` ["1", "2", "3"]

  it "counts for loops" $ do
    parsed <- parseFileQuiet goParser "test/fixtures/go/matching/for.go"
    let matched = recursively @[] @(Term _ _) loopMatcher parsed
    length matched `shouldBe` 2
