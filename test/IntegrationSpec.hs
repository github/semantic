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

spec :: Spec
spec = parallel $ do
  it "lists example fixtures" $ do
    examples "test/fixtures/go/" `shouldNotReturn` []
    examples "test/fixtures/ruby/" `shouldNotReturn` []

  describe "go" $ runTestsIn "test/fixtures/go/"
  describe "ruby" $ runTestsIn "test/fixtures/ruby/"

  where
    runTestsIn :: FilePath -> SpecWith ()
    runTestsIn directory = do
      examples <- runIO $ examples directory
      traverse_ runTest examples
    runTest ParseExample{..} = it ("parses " <> file) $ testParse file parseOutput
    runTest DiffExample{..} = it ("diffs " <> diffOutput) $ testDiff (Renderer.sExpression TreeOnly) (both fileA fileB) diffOutput

data Example = DiffExample { fileA :: FilePath, fileB :: FilePath, diffOutput :: FilePath }
             | ParseExample { file :: FilePath, parseOutput :: FilePath }
             deriving (Eq, Show)

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form:
-- |
-- | example-name.A.rb - The left hand side of the diff.
-- | example-name.B.rb - The right hand side of the diff.
-- |
-- | example-name.diffA-B.txt - The expected sexpression diff output for A -> B.
-- | example-name.diffB-A.txt - The expected sexpression diff output for B -> A.
-- |
-- | example-name.parseA.txt - The expected sexpression parse tree for example-name.A.rb
-- | example-name.parseB.txt - The expected sexpression parse tree for example-name.B.rb
examples :: FilePath -> IO [Example]
examples directory = do
  as <- globFor "*.A.*"
  bs <- globFor "*.B.*"
  sExpAs <- globFor "*.parseA.txt"
  sExpBs <- globFor "*.parseB.txt"
  sExpDiffsAB <- globFor "*.diffA-B.txt"
  sExpDiffsBA <- globFor "*.diffB-A.txt"

  let exampleDiff out name = DiffExample (lookupNormalized name as) (lookupNormalized name bs) out
  let exampleDiff' out name = DiffExample (lookupNormalized name bs) (lookupNormalized name as) out
  let exampleParse files out name = ParseExample (lookupNormalized name files) out

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ getExamples exampleDiff sExpDiffsAB keys
      <> getExamples exampleDiff' sExpDiffsBA keys
      <> getExamples (exampleParse as) sExpAs keys
      <> getExamples (exampleParse bs) sExpBs keys
  where
    -- Only returns examples if they exist
    getExamples f list = foldr (go f list) []
      where go f list name acc = case lookupNormalized' name list of
              Just out -> f out name : acc
              Nothing -> acc

    lookupNormalized :: FilePath -> [FilePath] -> FilePath
    lookupNormalized name xs = fromMaybe
      (panic ("cannot find " <> T.pack name <> " make sure .A, .B and exist." :: Text))
      (lookupNormalized' name xs)

    lookupNormalized' :: FilePath -> [FilePath] -> Maybe FilePath
    lookupNormalized' name = find ((== name) . normalizeName)

    globFor :: FilePath -> IO [FilePath]
    globFor p = globDir1 (compile p) directory

-- | Given a test name like "foo.A.js", return "foo".
normalizeName :: FilePath -> FilePath
normalizeName path = dropExtension $ dropExtension path

testParse :: FilePath -> FilePath -> Expectation
testParse path expectedOutput = do
  source <- readAndTranscodeFile path
  let blob = sourceBlob source path
  term <- parserWithSource path blob
  let actual = (Verbatim . stripWhitespace) $ printTerm term 0 TreeOnly
  expected <- (Verbatim . stripWhitespace) <$> readFile expectedOutput
  actual `shouldBe` expected

testDiff :: Renderer (Record '[Range, Category, SourceSpan]) -> Both FilePath -> FilePath -> Expectation
testDiff renderer paths diff = do
  sources <- traverse readAndTranscodeFile paths
  diff' <- diffFiles parser renderer (sourceBlobs sources)
  let actual = (Verbatim . stripWhitespace. concatOutputs . pure) diff'
  expected <- (Verbatim . stripWhitespace) <$> readFile diff
  actual `shouldBe` expected
  where
    parser = parserForFilepath (fst paths)
    sourceBlobs sources = Source.SourceBlob <$> sources <*> pure mempty <*> paths <*> pure (Just Source.defaultPlainBlob)

stripWhitespace :: Text -> Text
stripWhitespace = T.foldl' go T.empty
  where go acc x | x `elem` [' ', '\t', '\n'] = acc
                 | otherwise = T.snoc acc x

-- | A wrapper around `Text` with a more readable `Show` instance.
newtype Verbatim = Verbatim Text
  deriving (Eq, NFData)

instance Show Verbatim where
  showsPrec _ (Verbatim text) = ('\n':) . (T.unpack text ++)
