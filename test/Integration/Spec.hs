module Integration.Spec (spec) where

import Data.Foldable (find, traverse_, for_)
import Data.List (union, concat, transpose)
import qualified Data.ByteString as B
import System.FilePath.Glob
import System.FilePath.Posix

import SpecHelpers

languages :: [FilePath]
languages = ["go", "javascript", "json", "python", "ruby", "typescript", "java"]

spec :: TaskConfig -> Spec
spec config = parallel $ do
  for_ languages $ \language -> do
    let dir = "test/fixtures" </> language </> "corpus"
    it (language <> " corpus exists") $ examples dir `shouldNotReturn` []
    describe (language <> " corpus") $ runTestsIn dir []

  where
    runTestsIn :: FilePath -> [(FilePath, String)] -> SpecWith ()
    runTestsIn directory pending = do
      examples <- runIO $ examples directory
      traverse_ (runTest pending) examples
    runTest pending ParseExample{..} = it ("parses " <> file) $ maybe (testParse config file parseOutput) pendingWith (lookup parseOutput pending)
    runTest pending DiffExample{..} = it ("diffs " <> diffOutput) $ maybe (testDiff config (both fileA fileB) diffOutput) pendingWith (lookup diffOutput pending)

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

testParse :: TaskConfig -> FilePath -> FilePath -> Expectation
testParse config path expectedOutput = do
  actual <- verbatim <$> parseFilePath config path
  expected <- verbatim <$> B.readFile expectedOutput
  actual `shouldBe` expected

testDiff :: TaskConfig -> Both FilePath -> FilePath -> Expectation
testDiff config paths expectedOutput = do
  actual <- verbatim <$> diffFilePaths config paths
  expected <- verbatim <$> B.readFile expectedOutput
  actual `shouldBe` expected
