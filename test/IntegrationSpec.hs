{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
module IntegrationSpec where

import Category as C
import Data.Functor.Both
import Data.Record
import qualified Data.Text as T
import GHC.Show (Show(..))
import Data.List (union)
import Diffing
import Info
import Parse
import Prologue hiding (fst, snd)
import Renderer
import Renderer.SExpression as Renderer
import Source
import System.FilePath
import System.FilePath.Glob
import Test.Hspec (Spec, describe, it, SpecWith, runIO, parallel)
import Test.Hspec.Expectations.Pretty
import Unsafe (unsafeFromJust)

spec :: Spec
spec = parallel $ do
  it "lists example fixtures" $ do
    examples "test/corpus/sexpression/ruby/" `shouldNotReturn` []

  describe "should produce the correct diff" $ runTestsIn "test/corpus/sexpression/ruby/" shouldBe

  where
    runTestsIn :: FilePath -> (Maybe Verbatim -> Maybe Verbatim -> Expectation) -> SpecWith ()
    runTestsIn directory matcher = do
      paths <- runIO $ examples directory
      let tests = correctTests =<< paths
      traverse_ (\ (formatName, renderer, paths, output) ->
        it (maybe "/dev/null" normalizeName (uncurry (<|>) (runJoin paths)) ++ " (" ++ formatName ++ ")") $ testDiff renderer paths output matcher) tests

    correctTests paths@(_, _, Nothing) = testsForPaths paths
    correctTests paths = filter (\(_, _, _, output) -> isJust output) $ testsForPaths paths
    testsForPaths (aPath, bPath, sexpression) = [ ("sexpression", Renderer.sExpression TreeOnly, paths, sexpression) ]
      where paths = both aPath bPath

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.sexpression.js". Diffs are not
-- | required as the test may be verifying that the inputs don't crash.
-- |
-- | file.A.rb
-- | file.B.rb
-- | file.A.sexpression.rb
-- | file.B.sexpression.rb
-- | file.sexpression.rb
examples :: FilePath -> IO [(Maybe FilePath, Maybe FilePath, Maybe FilePath)]
examples directory = do
  as <- globFor "*.A.*"
  bs <- globFor "*.B.*"
  sexpressions <- globFor "*.sexpression.*"

  let lookupName name = (lookupNormalized name as, lookupNormalized name bs, lookupNormalized name sexpressions)

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ lookupName <$> keys
  where
    lookupNormalized name = find $ (== name) . normalizeName
    globFor :: FilePath -> IO [FilePath]
    globFor p = globDir1 (compile p) directory

-- | Given a test name like "foo.A.js", return "foo.js".
normalizeName :: FilePath -> FilePath
normalizeName path = dropExtension $ dropExtension path

-- | Given file paths for A, B, and, optionally, a diff, return whether diffing
-- | the files will produce the diff. If no diff is provided, then the result
-- | is true, but the diff will still be calculated.
testDiff :: Renderer (Record '[Cost, Range, Category, SourceSpan]) -> Both (Maybe FilePath) -> Maybe FilePath -> (Maybe Verbatim -> Maybe Verbatim -> Expectation) -> Expectation
testDiff renderer paths diff matcher = do
  sources <- traverse (traverse readAndTranscodeFile) paths
  actual <- fmap (Verbatim . stripWhitespace) <$> traverse ((pure . concatOutputs . pure) <=< diffFiles' sources) parser
  case diff of
    Nothing -> matcher actual actual
    Just file -> do
      expected <- (Verbatim . stripWhitespace) <$> readFile file
      matcher actual (Just expected)
  where
    diffFiles' sources parser = diffFiles parser renderer (sourceBlobs sources paths)
    parser = parserWithCost <$> runBothWith (<|>) paths

    sourceBlobs :: Both (Maybe Source) -> Both (Maybe FilePath) -> Both SourceBlob
    sourceBlobs sources paths = case runJoin paths of
      (Nothing, Nothing) -> Join (emptySourceBlob "", emptySourceBlob "")
      (Nothing, Just filepath) -> Join (emptySourceBlob "", sourceBlob (unsafeFromJust $ snd sources) filepath)
      (Just filepath, Nothing) ->  Join (sourceBlob (unsafeFromJust $ fst sources) filepath, emptySourceBlob "")
      (Just path1, Just path2) ->  Join (sourceBlob (unsafeFromJust $ fst sources) path1, sourceBlob (unsafeFromJust $ snd sources) path2)

    stripWhitespace :: Text -> Text
    stripWhitespace = T.foldl' go T.empty
      where go acc x | x == ' ' = acc
                     | x == '\n' = acc
                     | otherwise = T.snoc acc x

-- | A wrapper around `Text` with a more readable `Show` instance.
newtype Verbatim = Verbatim Text
  deriving (Eq, NFData)

instance Show Verbatim where
  showsPrec _ (Verbatim text) = ('\n':) . (T.unpack text ++)
