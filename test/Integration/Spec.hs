{-# LANGUAGE ImplicitParams, LambdaCase #-}
module Integration.Spec (testTree) where

import Control.Exception (throw)
import Data.Foldable (find)
import Data.List (union, concat, transpose)
import qualified Data.ByteString.Lazy as BL
import System.FilePath.Glob
import System.IO.Unsafe

import SpecHelpers
import qualified System.Path as Path
import System.Path ((</>))

import Test.Tasty
import Test.Tasty.Golden

languages :: [Path.RelDir]
languages = fmap Path.relDir ["go", "javascript", "json", "python", "ruby", "typescript", "tsx"]

testTree :: (?session :: TaskSession) => TestTree
testTree = testGroup "Integration (golden tests)" $ fmap testsForLanguage languages

testsForLanguage :: (?session :: TaskSession) => Path.RelDir -> TestTree
testsForLanguage language = do
  let dir = Path.relDir "test/fixtures" </> language </> Path.relDir "corpus"
  let items = unsafePerformIO (examples dir)
  localOption (mkTimeout 3000000) $ testGroup (Path.toString language) $ fmap testForExample items
{-# NOINLINE testsForLanguage #-}

data Example = DiffExample Path.RelFile Path.RelFile Path.RelFile
             | ParseExample Path.RelFile Path.RelFile
             deriving (Eq, Show)

testForExample :: (?session :: TaskSession) => Example -> TestTree
testForExample = \case
  DiffExample fileA fileB diffOutput ->
    goldenVsStringDiff
      ("diffs " <> Path.toString diffOutput)
      (\ref new -> ["git", "diff", ref, new])
      (Path.toString diffOutput)
      (BL.fromStrict <$> diffFilePaths ?session fileA fileB)
  ParseExample file parseOutput ->
    goldenVsStringDiff
      ("parses " <> Path.toString parseOutput)
      (\ref new -> ["git", "diff", ref, new])
      (Path.toString parseOutput)
      (parseFilePath ?session file >>= either throw (pure . BL.fromStrict))


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
examples :: Path.RelDir -> IO [Example]
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

    lookupNormalized :: Path.RelFile -> [Path.RelFile] -> Path.RelFile
    lookupNormalized name xs = fromMaybe
      (error ("cannot find " <> Path.toString name <> " make sure .A, .B and exist."))
      (lookupNormalized' name xs)

    lookupNormalized' :: Path.RelFile -> [Path.RelFile] -> Maybe Path.RelFile
    lookupNormalized' name = find ((== name) . normalizeName)

    globFor :: String -> IO [Path.RelFile]
    globFor p = fmap Path.relFile <$> globDir1 (compile p) (Path.toString directory)

-- | Given a test name like "foo.A.js", return "foo".
normalizeName :: Path.RelFile -> Path.RelFile
normalizeName = Path.dropExtension . Path.dropExtension
