module Main (main) where

import           Control.Monad
import           Control.Exception (displayException)
import qualified Data.ByteString as B
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable
import           Data.Maybe
import           Data.Either
import           Data.List
import           Data.Project (file)
import           Data.Void
import           Rendering.Renderer
import           Semantic.Config (Config (..), Options (..), defaultOptions)
import qualified Semantic.IO as IO
import           Semantic.Parse
import           Semantic.Task
import           Semantic.Util (TaskConfig (..))
import           System.Exit (die)
import           System.FilePath.Glob
import           System.FilePath.Posix
import           System.Directory
import           System.Process
import           Test.Hspec
import           Parsing.Parser
import           Data.Quieterm


main :: IO ()
main = withOptions opts $ \ config logger statter -> hspec . parallel $ do
  let args = TaskConfig config logger statter

  runIO setupExampleRepos

  for_ languages $ \ (lang, ext) -> do
    let tsDir = languagesDir </> lang </> ("vendor/tree-sitter-" <> lang)
    parallel . describe lang $ parseExamples args lang ext tsDir

  where
    parseExamples args lang ext tsDir = do
      knownFailures <- runIO $ BC.lines <$> (knownFailuresFile tsDir >>= B.readFile)
      let knownFailures' = (tsDir </>) . BC.unpack <$> knownFailures
      files <- runIO $ globDir1 (compile ("**/*" <> ext)) (tsDir </> "examples")
      for_ files $ \file -> it file $ do
        res <- parseFilePath args file
        if file `elem` knownFailures'
          then pendingWith $ "Known parse failures " <> show (const "Assignment: OK" <$> res)
          else res `shouldSatisfy` isRight

    setupExampleRepos = readProcess "script/setup-example-repos" mempty mempty >>= print
    opts = defaultOptions { optionsFailOnWarning = True, optionsLogLevel = Nothing }

    knownFailuresFile tsDir = do
      let f = tsDir </> "script/known_failures.txt"
      exists <- doesFileExist f
      if exists
        then pure f
        else pure $ tsDir </> "script/known-failures.txt"

languages :: [(FilePath, FilePath)]
languages =
  [ ("go", ".go")
  , ("python", ".py")
  , ("ruby", ".rb")
  , ("typescript", ".ts")

  -- TODO: Known failures are a bit more complicated or not in conventional format
  -- , ("java", ".java")
  -- , ("haskell", ".hs")

  -- , ("javascript", ".js") -- TODO: Actually tests javascript
  -- , ("php", ".php") -- TODO: No parse-examples in tree-sitter yet
  ]

parseFilePath :: TaskConfig -> FilePath -> IO (Either String ())
parseFilePath (TaskConfig config logger statter) path = do
  blob <- IO.readFile (file path)
  case blob of
    Just blob -> parse blob
    Nothing -> pure $ Left "readFile failed"
  where parse blob = either (Left . displayException) (const (Right ())) <$> runTaskWithConfig config logger statter (runParse' blob)

languagesDir :: FilePath
languagesDir = "vendor/haskell-tree-sitter/languages"
