{-# LANGUAGE OverloadedLists #-}
module Analysis.PHP.Spec (spec) where

import Data.Abstract.Value
import Data.Map
import Data.Map.Monoidal as Map

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "PHP" $ do
    it "evaluates include and require" $ do
      env <- evaluate "main.php"
      env `shouldBe` [ (name "foo", addr 0)
                     , (name "bar", addr 1) ]

    it "evaluates include_once and require_once" $ do
      env <- evaluate "main_once.php"
      env `shouldBe` [ (name "foo", addr 0)
                     , (name "bar", addr 1) ]

    it "evaluates namespaces" $ do
      ((_, env), Heap heap) <- evaluate' "namespaces.php"
      env `shouldBe` [ (name "NS1", addr 0)
                     , (name "Foo", addr 6) ]
      Map.lookup (Precise 0) heap `shouldBe` ns "NS1" [ (name "Sub1", addr 1)
                                                      , (name "b", addr 4)
                                                      , (name "c", addr 5)
                                                      ]
      Map.lookup (Precise 1) heap `shouldBe` ns "Sub1" [ (name "Sub2", addr 2) ]
      Map.lookup (Precise 2) heap `shouldBe` ns "Sub2" [ (name "f", addr 3) ]

  where
    ns n = Just . Latest . Just . injValue . Namespace (name n)
    addr = Address . Precise
    fixtures = "test/fixtures/php/analysis/"
    evaluate entry = snd . fst <$> evaluate' entry
    evaluate' entry = fst . fst . fst <$>
      evaluateFiles phpParser
      [ fixtures <> entry
      , fixtures <> "foo.php"
      , fixtures <> "bar.php"
      ]
