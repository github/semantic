module Data.Abstract.Path.Spec(spec) where

import Data.Abstract.Path
import SpecHelpers
import qualified System.Path as Path

spec :: Spec
spec = parallel $
  describe "joinPaths" $ do
    it "joins empty paths" $
      go (Path.currentDir) Path.currentDir $ Path.relDir "."
    it "joins relative paths" $
      go (Path.relDir "a/b") (Path.relFile "./c") $ Path.relFile "a/b/c"
    it "joins absolute paths" $
      go (Path.absDir "/a/b") (Path.relDir "c" ) $ Path.absDir "/a/b/c"
    it "walks up directories for ../" $
      go (Path.relDir "a/b") (Path.relFile "../c") $ Path.relFile "a/c"
    it "walks up directories for multiple ../" $
      go (Path.relDir "a/b") (Path.relFile "../../c") $ Path.relFile "c"
    it "stops walking at top directory" $
      go (Path.relDir "a/b") (Path.relFile "../../../c" ) $ Path.relFile "c"
  where
    go x y z = joinPaths (Path.toAbsRel x) y `shouldBe` Path.toAbsRel z
