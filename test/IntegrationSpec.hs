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
import qualified Data.Functor.Both as Both
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

  describe "should produce the correct ruby diffs" $ runTestsIn "test/corpus/sexpression/ruby/"

  where
    runTestsIn :: FilePath -> SpecWith ()
    runTestsIn directory = do
      examples <- runIO $ examples directory
      let tests = testsForPaths =<< examples
      traverse_ (\ (formatName, renderer, files, output) ->
        it (normalizeName (Both.fst files) ++ " (" ++ formatName ++ ")") $ testDiff renderer files output) tests

    testsForPaths (aPath, bPath, sexpression) = [ ("sexpression", Renderer.sExpression TreeOnly, paths, sexpression) ]
        where paths = both aPath bPath

data Example = DiffExample { fileA :: FilePath, fileB :: FilePath, diffOutput :: FilePath }
             | ParseExample { file :: FilePath, parseOutput :: FilePath }

-- | Return all the examples from the given directory. Examples are expected to
-- | have the form "foo.A.js", "foo.B.js", "foo.sexpression.js".
-- |
-- | file.A.rb
-- | file.B.rb
-- |
-- | file.sexpression.txt
-- |
-- | file.sexpressionA.txt
-- | file.sexpressionB.txt

examples :: FilePath -> IO [(FilePath, FilePath, FilePath)]
examples directory = do
  as <- globFor "*.A.*"
  bs <- globFor "*.B.*"
  sexpressions <- globFor "*[^AB].sexpression.*"

  let lookupName name = (lookupNormalized name as, lookupNormalized name bs, lookupNormalized name sexpressions)

  let keys = (normalizeName <$> as) `union` (normalizeName <$> bs)
  pure $ lookupName <$> keys
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
      where go acc x | x == ' ' = acc
                     | x == '\n' = acc
                     | otherwise = T.snoc acc x

-- | A wrapper around `Text` with a more readable `Show` instance.
newtype Verbatim = Verbatim Text
  deriving (Eq, NFData)

instance Show Verbatim where
  showsPrec _ (Verbatim text) = ('\n':) . (T.unpack text ++)
