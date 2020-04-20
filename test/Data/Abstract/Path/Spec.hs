module Data.Abstract.Path.Spec(spec) where

import Data.Abstract.Path
import SpecHelpers

spec :: Spec
spec = parallel $
  describe "joinPaths" $ do
    it "joins empty paths" $
      joinUntypedPaths "" "" `shouldBe` "."
    it "joins relative paths" $
      joinUntypedPaths "a/b" "./c" `shouldBe` "a/b/c"
    it "joins absolute paths" $
      joinUntypedPaths "/a/b" "c" `shouldBe` "/a/b/c"
    it "walks up directories for ../" $
      joinUntypedPaths "a/b" "../c" `shouldBe` "a/c"
    it "walks up directories for multiple ../" $
      joinUntypedPaths "a/b" "../../c" `shouldBe` "c"
    it "stops walking at top directory" $
      joinUntypedPaths "a/b" "../../../c" `shouldBe` "c"
