{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module IntegrationSpec where

import Command.Diff
import Command.Parse
import Data.Functor.Both
import Data.Record
import qualified Data.Text as T
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8)
import GHC.Show (Show(..))
import Data.List (union, concat, transpose)
import Info
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
    examples "test/fixtures/javascript/" `shouldNotReturn` []
    examples "test/fixtures/ruby/" `shouldNotReturn` []
    examples "test/fixtures/typescript/" `shouldNotReturn` []

  describe "go" $ runTestsIn "test/fixtures/go/"
  describe "javascript" $ runTestsIn "test/fixtures/javascript/"
  describe "ruby" $ runTestsIn "test/fixtures/ruby/"
  describe "typescript" $ runTestsIn "test/fixtures/typescript/"

  where
    runTestsIn :: FilePath -> SpecWith ()
    runTestsIn directory = do
      examples <- runIO $ examples directory
      traverse_ runTest examples
    runTest ParseExample{..} = it ("parses " <> file) $ testParse file parseOutput
    runTest DiffExample{..} = it ("diffs " <> diffOutput) $ testDiff ((SExpressionOutput .) . Renderer.sExpression TreeOnly) (both fileA fileB) diffOutput

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
  sExpDiffsAddA <- globFor "*.diff+A.txt"
  sExpDiffsRemoveA <- globFor "*.diff-A.txt"
  sExpDiffsAddB <- globFor "*.diff+B.txt"
  sExpDiffsRemoveB <- globFor "*.diff-B.txt"
  sExpDiffsAB <- globFor "*.diffA-B.txt"
  sExpDiffsBA <- globFor "*.diffB-A.txt"

  let exampleDiff lefts rights out name = DiffExample (lookupNormalized name lefts) (lookupNormalized name rights) out
  let exampleAddDiff files out name = DiffExample "" (lookupNormalized name files) out
  let exampleRemoveDiff files out name = DiffExample (lookupNormalized name files) "" out
  let exampleParse files out name = ParseExample (lookupNormalized name files) out

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ merge [ getExamples (exampleParse as) sExpAs keys
               , getExamples (exampleParse bs) sExpBs keys
               , getExamples (exampleAddDiff as) sExpDiffsAddA keys
               , getExamples (exampleRemoveDiff as) sExpDiffsRemoveA keys
               , getExamples (exampleAddDiff bs) sExpDiffsAddB keys
               , getExamples (exampleRemoveDiff bs) sExpDiffsRemoveB keys
               , getExamples (exampleDiff as bs) sExpDiffsAB keys
               , getExamples (exampleDiff bs as) sExpDiffsBA keys ]
  where
    merge = concat . transpose
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
  term <- parserForType (toS (takeExtension path)) blob
  let actual = (Verbatim . stripWhitespace) $ printTerm term 0 TreeOnly
  expected <- (Verbatim . stripWhitespace) <$> B.readFile expectedOutput
  actual `shouldBe` expected

testDiff :: Renderer (Record DefaultFields) -> Both FilePath -> FilePath -> Expectation
testDiff renderer paths expectedOutput = do
  sources <- traverse readAndTranscodeFile' paths
  diff <- diffFiles parser (sourceBlobs sources)
  let diffOutput = renderer (sourceBlobs sources) diff
  let actual = (Verbatim . stripWhitespace. concatOutputs . pure) diffOutput
  expected <- (Verbatim . stripWhitespace) <$> B.readFile expectedOutput
  actual `shouldBe` expected
  where
    parser = parserForFilepath filePath
    sourceBlobs sources = Source.SourceBlob <$> sources <*> pure mempty <*> paths <*> pure (Just Source.defaultPlainBlob)
    readAndTranscodeFile' path | Prologue.null path = pure Source.empty
                               | otherwise = readAndTranscodeFile path
    filePath = if fst paths /= "" then fst paths else snd paths

stripWhitespace :: ByteString -> ByteString
stripWhitespace = B.foldl' go B.empty
  where go acc x | x `B.elem` " \t\n" = acc
                 | otherwise = B.snoc acc x

-- | A wrapper around `ByteString` with a more readable `Show` instance.
newtype Verbatim = Verbatim ByteString
  deriving (Eq, NFData)

instance Show Verbatim where
  showsPrec _ (Verbatim byteString) = ('\n':) . (T.unpack (decodeUtf8 byteString) ++)
