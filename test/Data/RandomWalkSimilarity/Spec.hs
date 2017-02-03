{-# LANGUAGE DataKinds #-}
module Data.RandomWalkSimilarity.Spec where

import Category
import Data.Functor.Both
import Data.Functor.Listable
import Data.RandomWalkSimilarity
import Data.Record
import Data.String
import qualified Data.Vector as Vector
import Diff
import Info
import Patch
import Prologue
import Syntax
import Term
import Diffing (getLabel)
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = parallel $ do
  let positively = succ . abs
  describe "pqGramDecorator" $ do
    prop "produces grams with stems of the specified length" $
      \ (term, p, q) -> pqGramDecorator (rhead . headF) (positively p) (positively q) (unListableF term :: SyntaxTerm String '[Category]) `shouldSatisfy` all ((== positively p) . length . stem . rhead)

    prop "produces grams with bases of the specified width" $
      \ (term, p, q) -> pqGramDecorator (rhead . headF) (positively p) (positively q) (unListableF term :: SyntaxTerm String '[Category]) `shouldSatisfy` all ((== positively q) . length . base . rhead)

  describe "featureVectorDecorator" $ do
    prop "produces a vector of the specified dimension" $
      \ (term, p, q, d) -> featureVectorDecorator (rhead . headF) (positively p) (positively q) (positively d) (unListableF term :: SyntaxTerm String '[Category]) `shouldSatisfy` all ((== positively d) . length . rhead)

  describe "rws" $ do
    prop "produces correct diffs" $
      \ (as, bs) -> let tas = decorate <$> (unListableF <$> as :: [SyntaxTerm String '[Category]])
                        tbs = decorate <$> (unListableF <$> bs :: [SyntaxTerm String '[Category]])
                        root = cofree . ((Program :. Nil) :<) . Indexed
                        diff = wrap (pure (Program :. Nil) :< Indexed (stripDiff <$> rws compare getLabel tas tbs)) in
        (beforeTerm diff, afterTerm diff) `shouldBe` (Just (root (stripTerm <$> tas)), Just (root (stripTerm <$> tbs)))

    it "produces unbiased insertions within branches" $
      let (a, b) = (decorate (cofree ((StringLiteral :. Nil) :< Indexed [ cofree ((StringLiteral :. Nil) :< Leaf ("a" :: Text)) ])), decorate (cofree ((StringLiteral :. Nil) :< Indexed [ cofree ((StringLiteral :. Nil) :< Leaf "b") ]))) in
      fmap stripDiff (rws compare getLabel [ b ] [ a, b ]) `shouldBe` fmap stripDiff [ inserting a, copying b ]

  where compare :: (HasField fields Category, Functor f, Eq (Cofree f Category)) => Term f (Record fields) -> Term f (Record fields) -> Maybe (Diff f (Record fields))
        compare a b | (category <$> a) == (category <$> b) = Just (copying b)
                    | otherwise = if ((==) `on` category . extract) a b then Just (replacing a b) else Nothing
        copying :: Functor f => Cofree f (Record fields) -> Free (CofreeF f (Both (Record fields))) (Patch (Cofree f (Record fields)))
        copying = cata wrap . fmap pure
        decorate :: SyntaxTerm leaf '[Category] -> SyntaxTerm leaf '[FeatureVector, Category]
        decorate = defaultFeatureVectorDecorator (category . headF)
