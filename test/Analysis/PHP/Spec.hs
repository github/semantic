{-# LANGUAGE OverloadedLists #-}
module Analysis.PHP.Spec (spec) where

import Data.Abstract.Evaluatable (EvalError(..))
import qualified Language.PHP.Assignment as PHP
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      env <- environment . snd <$> evaluate "main.php"
      env `shouldBe` [ ("foo", addr 0)
                     , ("bar", addr 1) ]

    it "evaluates include_once and require_once" $ do
      env <- environment . snd <$> evaluate "main_once.php"
      env `shouldBe` [ ("foo", addr 0)
                     , ("bar", addr 1) ]

    it "evaluates namespaces" $ do
      res <- snd <$> evaluate "namespaces.php"
      environment res `shouldBe` [ ("NS1", addr 0)
                                 , ("Foo", addr 6) ]

      heapLookup (Address (Precise 0)) (heap res) `shouldBe` ns "NS1" [ ("Sub1", addr 1)
                                                                      , ("b", addr 4)
                                                                      , ("c", addr 5)
                                                                      ]
      heapLookup (Address (Precise 1)) (heap res) `shouldBe` ns "Sub1" [ ("Sub2", addr 2) ]
      heapLookup (Address (Precise 2)) (heap res) `shouldBe` ns "Sub2" [ ("f", addr 3) ]

  where
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = evalPHPProject (fixtures <> entry)
    evalPHPProject path = testEvaluating <$> evaluateProject phpParser Language.PHP Nothing path
