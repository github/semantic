module CorpusSpec where

import Diffing
import Unified

import Data.Bifunctor.Join
import qualified Data.ByteString.Char8 as B1
import Data.List as List
import Data.Map as Map
import Data.Set as Set
import Rainbow
import System.FilePath
import System.FilePath.Glob
import Test.Hspec

spec :: Spec
spec = do
  describe "crashers should not crash" $ runTestsIn "test/crashers/"
  describe "should produce the correct diff" $ runTestsIn "test/diffs/"

  it "lists example fixtures" $ do
    examples "test/crashers/" `shouldNotReturn` []
    examples "test/diffs/" `shouldNotReturn` []

  where
    runTestsIn directory = do
      tests <- runIO $ examples directory
      mapM_ (\ (a, b, diff) -> it (normalizeName $ oneOf a b) $ testDiff a b diff `shouldReturn` True) tests

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.diff.js". Diffs are not
-- | required as the test may be verifying that the inputs don't crash.
examples :: FilePath -> IO [(Maybe FilePath, Maybe FilePath, Maybe FilePath)]
examples directory = do
  aDict <- toDict <$> globFor "*.A.*"
  bDict <- toDict <$> globFor "*.B.*"
  diffDict <- toDict <$> globFor "*.diff.*"
  let keys = Set.unions $ keysSet <$> [aDict, bDict]
  return $ (\name -> (Map.lookup name aDict, Map.lookup name bDict, Map.lookup name diffDict)) <$> sort (Set.toList keys)

  where
    globFor :: String -> IO [FilePath]
    globFor p = globDir1 (compile p) directory
    toDict list = Map.fromList ((normalizeName <$> list) `zip` list)

-- | Given a test name like "foo.A.js", return "foo.js".
normalizeName :: FilePath -> FilePath
normalizeName path = addExtension (dropExtension $ dropExtension path) (takeExtension path)

-- | Given file paths for A, B, and, optionally, a diff, return whether diffing
-- | the files will produce the diff. If no diff is provided, then the result
-- | is true, but the diff will still be calculated.
testDiff :: Maybe FilePath -> Maybe FilePath -> Maybe FilePath -> IO Bool
testDiff a b diff = do
  let parser = parserForFilepath $ oneOf a b
  Just a <- return a
  Just b <- return b
  sources <- sequence $ readAndTranscodeFile <$> Join (a, b)
  chunks <- diffFiles parser unified (runJoin sources)
  let actual = mconcat $ chunksToByteStrings toByteStringsColors0 chunks
  case diff of
    Nothing -> return $ actual /= "<should not be a thing>"
    Just file -> do
      expected <- B1.readFile file
      return $ expected == actual

-- | Given two Maybes, at least one of which is known to be a Just, return the
-- | thing inside.
oneOf :: Maybe a -> Maybe a -> a
oneOf (Just a) _ = a
oneOf _ (Just b) = b
oneOf _ _ = error "oneOf expects one of its arguments to be Just a thing"
