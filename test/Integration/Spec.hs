{-# LANGUAGE ImplicitParams, LambdaCase, NamedFieldPuns #-}
module Integration.Spec (spec) where

import Control.Exception (throw)
import Data.Foldable (find, traverse_, for_)
import Data.List (union, concat, transpose)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import System.FilePath.Glob
import System.FilePath.Posix
import System.IO.Unsafe

import SpecHelpers

import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

languages :: [FilePath]
languages = ["go", "javascript", "json", "python", "ruby", "typescript", "tsx"]

spec :: TaskSession -> TestTree
spec config = let ?session = config in testGroup "Integration" $ fmap testsForLanguage languages

testsForLanguage :: (?session :: TaskSession) => FilePath -> TestTree
testsForLanguage language = do
  let dir = "test/fixtures" </> language </> "corpus"
  let items = unsafePerformIO (examples dir)
  testGroup language (fmap testForExample items)

data Example = DiffExample { fileA :: FilePath, fileB :: FilePath, diffOutput :: FilePath }
             | ParseExample { file :: FilePath, parseOutput :: FilePath }
             deriving (Eq, Show)

testForExample :: (?session :: TaskSession) => Example -> TestTree
testForExample = \case
  DiffExample{fileA, fileB, diffOutput} ->
    goldenVsStringDiff
      ("diffs " <> diffOutput)
      (\ref new -> ["git", "diff", ref, new])
      diffOutput
      (BL.fromStrict <$> diffFilePaths ?session (Both fileA fileB))
  ParseExample{file, parseOutput} -> testCase ("parses " <> file) (pure ())


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

  let exampleDiff lefts rights out name = DiffExample (lookupNormalized name lefts) (lookupNormalized name rights) out
  let exampleParse files out name = ParseExample (lookupNormalized name files) out

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ merge [ getExamples (exampleParse as) sExpAs keys
               , getExamples (exampleParse bs) sExpBs keys
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
      (error ("cannot find " <> name <> " make sure .A, .B and exist."))
      (lookupNormalized' name xs)

    lookupNormalized' :: FilePath -> [FilePath] -> Maybe FilePath
    lookupNormalized' name = find ((== name) . normalizeName)

    globFor :: FilePath -> IO [FilePath]
    globFor p = globDir1 (compile p) directory

-- | Given a test name like "foo.A.js", return "foo".
normalizeName :: FilePath -> FilePath
normalizeName path = dropExtension $ dropExtension path

