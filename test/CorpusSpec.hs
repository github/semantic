module CorpusSpec where

import Diffing
import PatchOutput
import Renderer
import Split
import Unified

import Control.DeepSeq
import Data.Bifunctor.Join
import qualified Data.ByteString.Char8 as B1
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Rainbow
import System.FilePath
import System.FilePath.Glob
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "crashers crash" $ runTestsIn "test/crashers-todo/" ((`shouldThrow` anyException) . return . uncurry (==))
  describe "crashers should not crash" $ runTestsIn "test/crashers/" (uncurry shouldBe)
  describe "todos are incorrect" $ runTestsIn "test/diffs-todo/" (uncurry shouldNotBe)
  describe "should produce the correct diff" $ runTestsIn "test/diffs/" (uncurry shouldBe)

  it "lists example fixtures" $ do
    examples "test/crashers/" `shouldNotReturn` []
    examples "test/diffs/" `shouldNotReturn` []

  where
    runTestsIn :: String -> ((String, String) -> Expectation) -> SpecWith ()
    runTestsIn directory matcher = do
      paths <- runIO $ examples directory
      let tests = correctTests =<< paths
      mapM_ (\ (formatName, renderer, a, b, output) -> it (normalizeName a ++ " (" ++ formatName ++ ")") $ testDiff renderer a b output matcher) tests

    correctTests :: (FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath) -> [(String, Renderer a String, FilePath, FilePath, Maybe FilePath)]
    correctTests paths@(_, _, Nothing, Nothing, Nothing) = testsForPaths paths
    correctTests paths = List.filter (\(_, _, _, _, output) -> isJust output) $ testsForPaths paths
    testsForPaths :: (FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath) -> [(String, Renderer a String, FilePath, FilePath, Maybe FilePath)]
    testsForPaths (a, b, patch, split, unified) = [ ("patch", PatchOutput.patch, a, b, patch), ("split", testSplit, a, b, split), ("unified", testUnified, a, b, unified) ]
    testSplit :: Renderer a String
    testSplit diff sources = TL.unpack $ Split.split diff sources
    testUnified :: Renderer a String
    testUnified diff sources = B1.unpack $ mconcat $ chunksToByteStrings toByteStringsColors0 $ unified diff sources


-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.unified.js". Diffs are not
-- | required as the test may be verifying that the inputs don't crash.
examples :: FilePath -> IO [(FilePath, FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath)]
examples directory = do
  as <- toDict <$> globFor "*.A.*"
  bs <- toDict <$> globFor "*.B.*"
  patches <- toDict <$> globFor "*.patch.*"
  splits <- toDict <$> globFor "*.split.*"
  unifieds <- toDict <$> globFor "*.unified.*"
  let keys = Set.unions $ keysSet <$> [as, bs]
  return $ (\name -> (as ! name, bs ! name, Map.lookup name patches, Map.lookup name splits, Map.lookup name unifieds)) <$> sort (Set.toList keys)

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
testDiff :: Renderer T.Text String -> FilePath -> FilePath -> Maybe FilePath -> ((String, String) -> Expectation) -> Expectation
testDiff renderer a b diff matcher = do
  let parser = parserForFilepath a
  sources <- sequence $ readAndTranscodeFile <$> Join (a, b)
  actual <- diffFiles parser renderer (runJoin sources)
  case diff of
    Nothing -> actual `deepseq` matcher (actual, actual)
    Just file -> do
      expected <- readFile file
      matcher (actual, expected)
