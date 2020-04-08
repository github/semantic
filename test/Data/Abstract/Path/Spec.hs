module Data.Abstract.Path.Spec(spec) where

import Data.Abstract.Path
import SpecHelpers

spec :: Spec
spec = parallel $
  describe "joinPaths" $ do
    it "joins empty paths" $
      joinPathf "" "" `shouldBe` "."
    it "joins relative paths" $
      joinPathf "a/b" "./c" `shouldBe` "a/b/c"
    it "joins absolute paths" $
      joinPathf "/a/b" "c" `shouldBe` "/a/b/c"
    it "walks up directories for ../" $
      joinPathf "a/b" "../c" `shouldBe` "a/c"
    it "walks up directories for multiple ../" $
      joinPathf "a/b" "../../c" `shouldBe` "c"
    it "stops walking at top directory" $
      joinPathf "a/b" "../../../c" `shouldBe` "c"
