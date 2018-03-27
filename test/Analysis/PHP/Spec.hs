{-# LANGUAGE OverloadedLists #-}
module Analysis.PHP.Spec (spec) where

import Data.Abstract.Value
import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      env <- environment . snd <$> evaluate "main.php"
      env `shouldBe` [ (name "foo", addr 0)
                     , (name "bar", addr 1) ]

    it "evaluates include_once and require_once" $ do
      env <- environment . snd <$> evaluate "main_once.php"
      env `shouldBe` [ (name "foo", addr 0)
                     , (name "bar", addr 1) ]

    it "evaluates namespaces" $ do
      res <- snd <$> evaluate "namespaces.php"
      environment res `shouldBe` [ (name "NS1", addr 0)
                                 , (name "Foo", addr 6) ]

      heapLookup (Address (Precise 0)) (heap res) `shouldBe` ns "NS1" [ (name "Sub1", addr 1)
                                                                      , (name "b", addr 4)
                                                                      , (name "c", addr 5)
                                                                      ]
      heapLookup (Address (Precise 1)) (heap res) `shouldBe` ns "Sub1" [ (name "Sub2", addr 2) ]
      heapLookup (Address (Precise 2)) (heap res) `shouldBe` ns "Sub2" [ (name "f", addr 3) ]

  where
    ns n = Just . Latest . Just . injValue . Namespace (name n)
    addr = Address . Precise
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = evaluateFiles phpParser
      [ fixtures <> entry
      , fixtures <> "foo.php"
      , fixtures <> "bar.php"
      ]
