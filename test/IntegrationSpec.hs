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
    examples "test/corpus/sexpression/ruby/" `shouldNotReturn` []

  describe "parse and diff ruby" $ runTestsIn "test/corpus/sexpression/ruby/"

  where
    runTestsIn :: FilePath -> SpecWith ()
    runTestsIn directory = do
      examples <- runIO $ examples directory
      traverse_ runTest examples
    runTest SExpressionParse{..} = it (file <> " (sexpression parse)") $ testParse file parseOutput
    runTest SExpressionDiff{..} = it (normalizeName fileA <> " (sexpression diff)") $ testDiff (Renderer.sExpression TreeOnly) (both fileA fileB) diffOutput

data Example = SExpressionDiff { fileA :: FilePath, fileB :: FilePath, diffOutput :: FilePath }
             | SExpressionParse { file :: FilePath, parseOutput :: FilePath }
             deriving (Eq, Show)

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form:
-- |
-- | file.A.rb - The left and right hand side of the diff.
-- | file.B.rb
-- |
-- | file.sexpression.txt - The expected sexpression output.
-- |
-- | file.sexpressionA.txt - The expected sexpression parse tree.
-- | file.sexpressionB.txt
examples :: FilePath -> IO [Example]
examples directory = do
  as <- globFor "*.A.*"
  bs <- globFor "*.B.*"
  sExpAs <- globFor "*.sexpressionA.*"
  sExpBs <- globFor "*.sexpressionB.*"
  sExpDiffs <- globFor "*[^AB].sexpression.*"

  let sExpDiff name = SExpressionDiff (lookupNormalized name as) (lookupNormalized name bs) (lookupNormalized name sExpDiffs)
  let sExpParseAs name = SExpressionParse (lookupNormalized name as) (lookupNormalized name sExpAs)
  let sExpParseBs name = SExpressionParse (lookupNormalized name bs) (lookupNormalized name sExpBs)

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ (sExpDiff <$> keys) <> (sExpParseAs <$> keys) <> (sExpParseBs <$> keys)
  where
    lookupNormalized :: FilePath -> [FilePath] -> FilePath
    lookupNormalized name xs = fromMaybe
      (panic ("cannot find " <> T.pack name <> " make sure .A, .B and .sexpression.txt exist." :: Text))
      (find ((== name) . normalizeName) xs)
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

testDiff :: Renderer (Record '[Cost, Range, Category, SourceSpan]) -> Both FilePath -> FilePath -> Expectation
testDiff renderer paths diff = do
  sources <- sequence $ readAndTranscodeFile <$> paths
  diff' <- diffFiles parser renderer (sourceBlobs sources)
  let actual = (Verbatim . stripWhitespace. concatOutputs . pure) diff'
  expected <- (Verbatim . stripWhitespace) <$> readFile diff
  actual `shouldBe` expected
  where
    parser = parserWithCost (fst paths)
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
