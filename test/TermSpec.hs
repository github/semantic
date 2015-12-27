module TermSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)

import Categorizable
import Interpreter
import Diff
import Syntax
import Term
import ArbitraryTerm

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Term" $ do
    prop "equality is reflexive" $
      \ a -> unTerm a == unTerm (a :: ArbitraryTerm String ())

  describe "Diff" $ do
    prop "equality is reflexive" $
      \ a b -> let diff = interpret comparable (unTerm a) (unTerm (b :: ArbitraryTerm String CategorySet)) in
        diff == diff

    prop "equal terms produce identity diffs" $
      \ a -> let term = unTerm (a :: ArbitraryTerm String CategorySet) in
        diffCost (interpret comparable term term) == 0
