{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
module Integration.Spec (testTree) where

import           Control.Exception (throw)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (find)
import           Data.List (transpose, union)
import           SpecHelpers
import           System.FilePath
import           System.FilePath.Glob
import           System.IO.Unsafe
import           Test.Tasty
import           Test.Tasty.Golden

languages :: [FilePath]
languages = ["go", "javascript", "json", "python", "ruby", "typescript", "tsx"]

testTree :: (?session :: TaskSession) => TestTree
testTree = testGroup "Integration (golden tests)" $ fmap testsForLanguage languages

testsForLanguage :: (?session :: TaskSession) => FilePath -> TestTree
testsForLanguage language = do
  let dir = "test/fixtures" </> language </> "corpus"
  let items = unsafePerformIO (examples dir)
  localOption (mkTimeout 3000000) $ testGroup language $ fmap testForExample items
{-# NOINLINE testsForLanguage #-}

data Example = ParseExample FilePath FilePath
             deriving (Eq, Show)

testForExample :: (?session :: TaskSession) => Example -> TestTree
testForExample (ParseExample file parseOutput) =
    goldenVsStringDiff
      ("parses " <> parseOutput)
      (\ref new -> ["git", "diff", ref, new])
      parseOutput
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
examples :: FilePath -> IO [Example]
examples directory = do
  as <- globFor "*.A.*"
  bs <- globFor "*.B.*"
  sExpAs <- globFor "*.parseA.txt"
  sExpBs <- globFor "*.parseB.txt"

  let exampleParse files out name = ParseExample (lookupNormalized name files) out

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ merge [ getExamples (exampleParse as) sExpAs keys
               , getExamples (exampleParse bs) sExpBs keys ]
  where
    merge = concat . transpose
    -- Only returns examples if they exist
    getExamples f list = foldr (go f list) []
      where go f list name acc = case lookupNormalized' name list of
              Just out -> f out name : acc
              Nothing  -> acc

    lookupNormalized :: FilePath -> [FilePath] -> FilePath
    lookupNormalized name xs = fromMaybe
      (error ("cannot find " <> name <> " make sure .A, .B and exist."))
      (lookupNormalized' name xs)

    lookupNormalized' :: FilePath -> [FilePath] -> Maybe FilePath
    lookupNormalized' name = find ((== name) . normalizeName)

    globFor :: String -> IO [FilePath]
    globFor p = globDir1 (compile p) directory

-- | Given a test name like "foo.A.js", return "foo".
normalizeName :: FilePath -> FilePath
normalizeName = dropExtension . dropExtension
