module Data.Abstract.Environment.Spec where

import Prelude hiding (head)
import SpecHelpers

import Data.Abstract.Environment
import Data.Abstract.Address.Precise

spec :: Spec
spec = describe "Environment" $ do
  let bright = push (insertEnv (name "foo") (Precise 0) lowerBound)
  let shadowed = insertEnv (name "foo") (Precise 1) bright

  it "can extract bindings" $
    pairs (head shadowed) `shouldBe` [("foo", Precise 1)]

  it "should extract the outermost binding given shadowing" $
    lookupEnv' (name "foo") shadowed `shouldBe` Just (Precise 1)

  it "can delete bindings" $
    delete (name "foo") shadowed `shouldBe` Environment (pure lowerBound)
