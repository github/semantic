{-# LANGUAGE TypeFamilies, TypeOperators #-}

module Control.Rewriting.Spec (spec) where

import SpecHelpers

import           Control.Abstract.Matching as Matching
import           Control.Rewriting as Rewriting
import qualified Data.ByteString as B
import           Data.Either
import qualified Data.Source as Source
import           Data.Sum
import qualified Data.Syntax.Literal as Literal
import           Language.JSON.PrettyPrint
import Data.History as History

import Data.Text (Text)

-- import           Reprinting.Pipeline

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

isHi :: ( Literal.TextElement :< fs
        , ann ~ Record (History : fields)
        ) => Matcher (Term (Sum fs) ann) Text
isHi = match Literal.textElementContent (Matching.target <* ensure (== "'hi'"))

spec :: Spec
spec = describe "rewriting" $ do
  it "should add keys to JSON values" $ do
    let path = "test/fixtures/json/rewriting/add_keys.json"

    bytes <- Source.fromUTF8 <$> B.readFile path
    json <- parseFile jsonParser path

    refactored <-
      case applyPure (somewhere' onTrees markRefactored) () (History.mark Unmodified json) of
        Left  l -> fail (show l)
        Right r -> pure r

    length (runMatcher @[] isHi refactored) `shouldBe` 1

    -- let res = runReprinter bytes defaultJSONPipeline refactored
    -- expected <- Source.fromUTF8 <$> B.readFile "test/fixtures/json/rewriting/add_keys_expected.json"
    -- res `shouldBe` Right expected
