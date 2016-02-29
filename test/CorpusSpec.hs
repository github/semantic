module CorpusSpec where

import Diffing
import Renderer
import qualified Renderer.Patch as P
import qualified Renderer.Split as Split

import qualified Source as S
import Control.DeepSeq
import Data.Functor.Both
import qualified Data.ByteString.Char8 as B1
import Data.List as List
import Data.Map as Map
import Data.Maybe
import Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath
import System.FilePath.Glob
import Test.Hspec

spec :: Spec
spec = parallel $ do
  -- describe "crashers crash" $ runTestsIn "test/crashers-todo/" ((`shouldThrow` anyException) . return)
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
      mapM_ (\ (formatName, renderer, paths, output) -> it (normalizeName (fst $ runBoth paths) ++ " (" ++ formatName ++ ")") $ testDiff renderer paths output matcher) tests

    correctTests :: (Both FilePath, Maybe FilePath, Maybe FilePath) -> [(String, Renderer a String, Both FilePath, Maybe FilePath)]
    correctTests paths@(_, Nothing, Nothing) = testsForPaths paths
    correctTests paths = List.filter (\(_, _, _, output) -> isJust output) $ testsForPaths paths
    testsForPaths :: (Both FilePath, Maybe FilePath, Maybe FilePath) -> [(String, Renderer a String, Both FilePath, Maybe FilePath)]
    testsForPaths (paths, patch, split) = [ ("patch", P.patch, paths, patch), ("split", testSplit, paths, split) ]
    testSplit :: Renderer a String
    testSplit diff sources = TL.unpack $ Split.split diff sources


-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.patch.js". Diffs are not
-- | required as the test may be verifying that the inputs don't crash.
examples :: FilePath -> IO [(Both FilePath, Maybe FilePath, Maybe FilePath)]
examples directory = do
  as <- toDict <$> globFor "*.A.*"
  bs <- toDict <$> globFor "*.B.*"
  patches <- toDict <$> globFor "*.patch.*"
  splits <- toDict <$> globFor "*.split.*"
  let keys = Set.unions $ keysSet <$> [as, bs]
  return $ (\name -> (Both (as ! name, bs ! name), Map.lookup name patches, Map.lookup name splits)) <$> sort (Set.toList keys)

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
testDiff :: Renderer T.Text String -> Both FilePath -> Maybe FilePath -> ((String, String) -> Expectation) -> Expectation
testDiff renderer paths diff matcher = do
  let parser = parserForFilepath (fst $ runBoth paths)
  sources <- sequence $ readAndTranscodeFile <$> paths
  let sourceBlobs = Both (S.SourceBlob, S.SourceBlob) <*> sources <*> Both (mempty, mempty) <*> paths
  actual <- diffFiles parser renderer sourceBlobs
  case diff of
    Nothing -> actual `deepseq` matcher (actual, actual)
    Just file -> do
      expected <- readFile file
      matcher (actual, expected)
