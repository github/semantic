{-# LANGUAGE DataKinds, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module CorpusSpec where

import Data.String
import Diffing
import Renderer
import qualified Renderer.JSON as J
import qualified Renderer.Patch as P
import qualified Renderer.Split as Split

import Category
import Control.DeepSeq
import Data.Functor.Both
import Data.List as List
import Data.Map as Map
import Data.Record
import Data.Set as Set
import qualified Data.Text as T
import Info
import Prologue hiding (fst, snd)
import Range
import qualified Source as S
import System.FilePath
import System.FilePath.Glob
import Test.Hspec
import GHC.Show (Show(..))

spec :: Spec
spec = parallel $ do
  describe "crashers crash" . runTestsIn "test/crashers-todo/" $ \ a b -> a `deepseq` pure (a == b) `shouldThrow` anyException
  describe "crashers should not crash" $ runTestsIn "test/crashers/" shouldBe
  describe "todos are incorrect" $ runTestsIn "test/diffs-todo/" shouldNotBe
  describe "should produce the correct diff" $ runTestsIn "test/diffs/" shouldBe

  it "lists example fixtures" $ do
    examples "test/crashers/" `shouldNotReturn` []
    examples "test/diffs/" `shouldNotReturn` []

  where
    runTestsIn :: FilePath -> (Verbatim -> Verbatim -> Expectation) -> SpecWith ()
    runTestsIn directory matcher = do
      paths <- runIO $ examples directory
      let tests = correctTests =<< paths
      traverse_ (\ (formatName, renderer, paths, output) -> it (normalizeName (fst paths) <> " (" <> formatName <> ")") $ testDiff renderer paths output matcher) tests

    correctTests paths@(_, Nothing, Nothing, Nothing) = testsForPaths paths
    correctTests paths = List.filter (\(_, _, _, output) -> isJust output) $ testsForPaths paths
    testsForPaths (paths, json, patch, split) = [ ("json", J.json, paths, json), ("patch", P.patch, paths, patch), ("split", Split.split, paths, split) ]

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.patch.js". Diffs are not
-- | required as the test may be verifying that the inputs don't crash.
examples :: FilePath -> IO [(Both FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath)]
examples directory = do
  as <- toDict <$> globFor "*.A.*"
  bs <- toDict <$> globFor "*.B.*"
  jsons <- toDict <$> globFor "*.json.*"
  patches <- toDict <$> globFor "*.patch.*"
  splits <- toDict <$> globFor "*.split.*"
  let keys = Set.unions $ keysSet <$> [as, bs]
  pure $ (\name -> (both (as ! name) (bs ! name), Map.lookup name jsons, Map.lookup name patches, Map.lookup name splits)) <$> sort (Set.toList keys)
  where
    globFor :: FilePath -> IO [FilePath]
    globFor p = globDir1 (compile p) directory
    toDict list = Map.fromList ((normalizeName <$> list) `zip` list)

-- | Given a test name like "foo.A.js", return "foo.js".
normalizeName :: FilePath -> FilePath
normalizeName path = addExtension (dropExtension $ dropExtension path) (takeExtension path)

-- | Given file paths for A, B, and, optionally, a diff, return whether diffing
-- | the files will produce the diff. If no diff is provided, then the result
-- | is true, but the diff will still be calculated.
testDiff :: Renderer (Record '[Cost, Range, Category]) -> Both FilePath -> Maybe FilePath -> (Verbatim -> Verbatim -> Expectation) -> Expectation
testDiff renderer paths diff matcher = do
  sources <- sequence $ readAndTranscodeFile <$> paths
  actual <- Verbatim <$> diffFiles (decorateParser termCostDecorator parser) renderer (sourceBlobs sources)
  case diff of
    Nothing -> matcher actual actual
    Just file -> do
      expected <- Verbatim <$> readFile file
      matcher actual expected
  where parser = parserForFilepath (fst paths)
        sourceBlobs sources = pure S.SourceBlob <*> sources <*> pure mempty <*> paths <*> pure (Just S.defaultPlainBlob)

-- | A wrapper around `Text` with a more readable `Show` instance.
newtype Verbatim = Verbatim Text
  deriving (Eq, NFData)

instance Show Verbatim where
  showsPrec _ (Verbatim text) = ('\n':) . (T.unpack text ++)
