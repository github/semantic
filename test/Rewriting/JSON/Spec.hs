{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Rewriting.JSON.Spec (spec) where

import Prelude hiding (id, (.))
import SpecHelpers

import           Control.Category
import           Control.Rewriting as Rewriting
import qualified Data.ByteString as B
import           Data.History as History
import           Data.Sum
import qualified Data.Syntax.Literal as Literal
import           Data.Text (Text)
import           Language.JSON.PrettyPrint
import           Reprinting.Pipeline
import qualified Source.Source as Source

-- Adds a "hi": "bye" key-value pair to any empty Hash.
onTrees :: ( Literal.TextElement :< syn
          , Literal.Hash :< syn
          , Literal.KeyValue :< syn
          , Apply Functor syn
          , term ~ Term (Sum syn) History
          ) => Rule term
onTrees = do
  Literal.Hash els <- Rewriting.target >>= guardTerm
  guard (null els)
  k <- create $ Literal.TextElement "\"hi\""
  v <- create $ Literal.TextElement "\"bye\""
  pair <- create $ Literal.KeyValue k v
  create (Literal.Hash (pair : els))

-- Matches only "hi" string literals.
isHi :: ( Literal.TextElement :< fs
        ) => Rewrite (Term (Sum fs) History) Text
isHi = enter Literal.textElementContent
       >>> ensure (== "\"hi\"")

spec :: Spec
spec = describe "rewriting" $ do
  let path = "test/fixtures/json/rewriting/add_keys.json"

  bytes <- runIO $ Source.fromUTF8 <$> B.readFile path

  refactored <- runIO $ do
    json <- parseFileQuiet jsonParser path
    let result = rewrite @Maybe (History.mark Unmodified json) (topDownAny onTrees)
    maybe (fail "rewrite failed") pure result

  it "should add keys to JSON values" $ do
    length (recursively @[] isHi refactored) `shouldBe` 1

  it "should round-trip correctly" $ do
    let res = runReprinter bytes defaultJSONPipeline refactored
    expected <- Source.fromUTF8 <$> B.readFile "test/fixtures/json/rewriting/add_keys_expected.json"
    res `shouldBe` Right expected
