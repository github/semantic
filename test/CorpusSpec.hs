{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module CorpusSpec where

import Category
import Control.DeepSeq
import Data.Functor.Both
import Data.List (union)
import Data.Record
import qualified Data.Text as T
import Diffing
import GHC.Show (Show(..))
import Info
import Prologue hiding (fst, snd, lookup)
import Parse
import Renderer
import qualified Renderer.JSON as J
import qualified Renderer.Patch as P
import qualified Renderer.Split as Split
import qualified Source as S
import System.FilePath
import System.FilePath.Glob
import Test.Hspec (Spec, describe, it, SpecWith, runIO, parallel)
import Test.Hspec.Expectations.Pretty
import Unsafe (unsafeFromJust)

spec :: Spec
spec = parallel $ do
  describe "crashers crash" . runTestsIn "test/crashers-todo/" $ \ a b ->
    a `deepseq` pure (a == b) `shouldThrow` anyException
  describe "crashers should not crash" $ runTestsIn "test/crashers/" shouldBe
  describe "todos are incorrect" $ runTestsIn "test/diffs-todo/" shouldNotBe
  describe "should produce the correct diff" $ runTestsIn "test/diffs/" shouldBe

  it "lists example fixtures" $ do
    examples "test/crashers/" `shouldNotReturn` []
    examples "test/diffs/" `shouldNotReturn` []

  where
    runTestsIn :: FilePath -> (Maybe Verbatim -> Maybe Verbatim -> Expectation) -> SpecWith ()
    runTestsIn directory matcher = do
      paths <- runIO $ examples directory
      let tests = correctTests =<< paths
      traverse_ (\ (formatName, renderer, paths, output) ->
        it (maybe "/dev/null" normalizeName (uncurry (<|>) (runJoin paths)) ++ " (" ++ formatName ++ ")") $ testDiff renderer paths output matcher) tests

    correctTests paths@(_, _, Nothing, Nothing, Nothing) = testsForPaths paths
    correctTests paths = filter (\(_, _, _, output) -> isJust output) $ testsForPaths paths
    testsForPaths (aPath, bPath, json, patch, split) = [ ("json", J.json, paths, json), ("patch", P.patch, paths, patch), ("split", Split.split, paths, split) ]
      where paths = both aPath bPath

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.patch.js". Diffs are not
-- | required as the test may be verifying that the inputs don't crash.
examples :: FilePath -> IO [(Maybe FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath)]
examples directory = do
  as <- globFor "*.A.*"
  bs <- globFor "*.B.*"
  jsons <- globFor "*.json.*"
  patches <- globFor "*.patch.*"
  splits <- globFor "*.split.*"

  let lookupName name = (lookupNormalized name as, lookupNormalized name bs, lookupNormalized name jsons, lookupNormalized name patches, lookupNormalized name splits)

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ lookupName <$> keys
  where
    lookupNormalized name = find $ (== name) . normalizeName
    globFor :: FilePath -> IO [FilePath]
    globFor p = globDir1 (compile p) directory

-- | Given a test name like "foo.A.js", return "foo.js".
normalizeName :: FilePath -> FilePath
normalizeName path = addExtension (dropExtension $ dropExtension path) (takeExtension path)

-- | Given file paths for A, B, and, optionally, a diff, return whether diffing
-- | the files will produce the diff. If no diff is provided, then the result
-- | is true, but the diff will still be calculated.
testDiff :: Renderer (Record '[Range, Category, SourceSpan]) -> Both (Maybe FilePath) -> Maybe FilePath -> (Maybe Verbatim -> Maybe Verbatim -> Expectation) -> Expectation
testDiff renderer paths diff matcher = do
  sources <- traverse (traverse readAndTranscodeFile) paths
  actual <- fmap Verbatim <$> traverse ((pure . concatOutputs . pure) <=< diffFiles' sources) parser
  case diff of
    Nothing -> matcher actual actual
    Just file -> do
      expected <- Verbatim <$> readFile file
      matcher actual (Just expected)
  where diffFiles' sources parser = diffFiles parser renderer (sourceBlobs sources paths)
        parser = parserForFilepath <$> runBothWith (<|>) paths
        sourceBlobs :: Both (Maybe (S.Source)) -> Both (Maybe FilePath) -> Both S.SourceBlob
        sourceBlobs sources paths = case runJoin paths of
          (Nothing, Nothing) -> Join (S.emptySourceBlob "", S.emptySourceBlob "")
          (Nothing, Just filepath) -> Join (S.emptySourceBlob "", S.sourceBlob (unsafeFromJust $ snd sources) filepath)
          (Just filepath, Nothing) ->  Join (S.sourceBlob (unsafeFromJust $ fst sources) filepath, S.emptySourceBlob "")
          (Just path1, Just path2) ->  Join (S.sourceBlob (unsafeFromJust $ fst sources) path1, S.sourceBlob (unsafeFromJust $ snd sources) path2)

-- | A wrapper around `Text` with a more readable `Show` instance.
newtype Verbatim = Verbatim Text
  deriving (Eq, NFData)

instance Show Verbatim where
  showsPrec _ (Verbatim text) = ('\n':) . (T.unpack text ++)
