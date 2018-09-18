{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Control.Rewriting.Spec (spec) where

import SpecHelpers

import qualified Data.ByteString as B
import           Data.Either
import           Data.Text (Text)

import           Control.Abstract.Matching as Matching
import           Control.Rewriting as Rewriting
import           Data.History as History
import qualified Data.Source as Source
import           Data.Sum
import qualified Data.Syntax.Literal as Literal
import           Language.JSON.PrettyPrint
import           Reprinting.Pipeline

-- Adds a "hi": "bye" key-value pair to any empty Hash.
onTrees :: ( Literal.TextElement :< syn
           , Literal.KeyValue :< syn
           , Apply Functor syn
           , term ~ Term (Sum syn) (Record (History : fields))
           ) => Rewrite (env, term) m (Literal.Hash term)
onTrees = do
  Literal.Hash els <- Rewriting.target
  guard (null els)
  k <- modified $ Literal.TextElement "\"hi\""
  v <- modified $ Literal.TextElement "\"bye\""
  pair <- modified $ (Literal.KeyValue k v)
  pure (Literal.Hash (pair : els))

-- Matches only "hi" string literals.
isHi :: ( Literal.TextElement :< fs
        , ann ~ Record (History : fields)
        ) => Matcher (Term (Sum fs) ann) Text
isHi = match Literal.textElementContent (Matching.target <* ensure (== "\"hi\""))

spec :: Spec
spec = describe "rewriting" $ do
  let path = "test/fixtures/json/rewriting/add_keys.json"

  bytes <- runIO $ Source.fromUTF8 <$> B.readFile path

  refactored <- runIO $ do
    json <- parseFile jsonParser path
    let result = applyPure (somewhere' onTrees markRefactored) () (History.mark Unmodified json)
    either (fail . show) pure result

  it "should add keys to JSON values" $ do
    length (runMatcher @[] isHi refactored) `shouldBe` 1

  it "should round-trip correctly" $ do
    let res = runReprinter bytes defaultJSONPipeline refactored
    expected <- Source.fromUTF8 <$> B.readFile "test/fixtures/json/rewriting/add_keys_expected.json"
    res `shouldBe` Right expected
