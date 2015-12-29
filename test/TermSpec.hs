module TermSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Fixed)
import Data.Text.Arbitrary ()

import Categorizable
<<<<<<< HEAD
import Control.Comonad.Cofree
import Control.Monad
import GHC.Generics
import qualified Data.List as List
import qualified Data.Set as Set
import qualified OrderedMap as Map
import Diff
import Interpreter
=======
import Interpreter
import Diff
>>>>>>> origin/master
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
