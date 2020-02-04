module Data.Abstract.Path.Spec(spec) where

import Data.Abstract.Path
import SpecHelpers

spec :: Spec
spec = parallel $
  describe "joinPaths" $ do
    it "joins empty paths" $
      joinPaths "" "" `shouldBe` "."
    it "joins relative paths" $
      joinPaths "a/b" "./c" `shouldBe` "a/b/c"
    it "joins absolute paths" $
      joinPaths "/a/b" "c" `shouldBe` "/a/b/c"
    it "walks up directories for ../" $
      joinPaths "a/b" "../c" `shouldBe` "a/c"
    it "walks up directories for multiple ../" $
      joinPaths "a/b" "../../c" `shouldBe` "c"
    it "stops walking at top directory" $
      joinPaths "a/b" "../../../c" `shouldBe` "c"
