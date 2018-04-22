{-# LANGUAGE OverloadedLists #-}
module Analysis.Go.Spec (spec) where

import Data.Abstract.Evaluatable (EvalError(..), runAnalysis)
import qualified Language.Go.Assignment as Go

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "evaluates Go" $ do
    it "imports and wildcard imports" $ do
      res <- snd <$> evaluate "main.go"
      environment res `shouldBe` [ ("foo", addr 0)
                                 , ("Bar", addr 2)
                                 , ("Rab", addr 3)
                                 , ("main", addr 4)
                                 ]

      heapLookup (Address (Precise 0)) (heap res) `shouldBe` ns "foo" [ ("New", addr 1) ]

    it "imports with aliases (and side effects only)" $ do
      res <- snd <$> evaluate "main1.go"
      environment res `shouldBe` [ ("f", addr 0)
                                 , ("main", addr 4) -- addr 4 is due to side effects of eval'ing `import _ "./bar"` which used addr 2 & 3. f defines New which got addr 1.
                                 ]

      heapLookup (Address (Precise 0)) (heap res) `shouldBe` ns "f" [ ("New", addr 1) ]

  where
    fixtures = "test/fixtures/go/analysis/"
    evaluate entry = evalGoProject (fixtures <> entry)
    evalGoProject path = runAnalysis @(TestEvaluating Go.Term) <$> evaluateProject goParser Nothing path
