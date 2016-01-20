{-# LANGUAGE RecordWildCards #-}
module Main where

import Interpreter
import qualified Parsers as P
import qualified PatchOutput
import Renderer
import Split
import Unified
import Source
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as B1
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TextIO
import qualified System.IO as IO
import Data.Bifunctor.Join
import Git.Libgit2
import Git.Types
import Git.Repository
import Data.Tagged
import Control.Monad.Reader
import System.Environment
import DiffOutput

-- | The available types of diff rendering.
data Format = Unified | Split | Patch

-- | The command line arguments to the application.
data Arguments = Arguments { format :: Format, output :: Maybe FilePath, shaA :: String, shaB :: String, filepaths :: [FilePath] }

-- | A parser for the application's command-line arguments.
arguments :: Parser Arguments
arguments = Arguments
  <$> (flag Split Unified (long "unified" <> help "output a unified diff")
  <|> flag Split Patch (long "patch" <> help "output a patch(1)-compatible diff")
  <|> flag' Split (long "split" <> help "output a split diff"))
  <*> optional (strOption (long "output" <> short 'o' <> help "output directory for split diffs, defaulting to stdout if unspecified"))
  <*> strArgument (metavar "SHA_A")
  <*> strArgument (metavar "SHA_B")
  <*> many (strArgument (metavar "FILE"))

main :: IO ()
main = do
  gitDir <- getEnv "GIT_DIR"
  arguments@Arguments{..} <- execParser opts
  let shas = Join (shaA, shaB)
  forM_ filepaths $ \filepath -> do
    sources <- sequence $ fetchFromGitRepo gitDir filepath <$> shas
    let parse = (P.parserForType . T.pack . takeExtension) filepath
    terms <- sequence $ parse <$> sources
    let replaceLeaves = breakDownLeavesByWord <$> sources
    printDiff arguments filepath (uncurry diffTerms . runJoin $ replaceLeaves <*> terms) (runJoin sources)
    where opts = info (helper <*> arguments)
            (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")

-- | Returns a file source given an absolute repo path, a relative file path, and the sha to look up.
fetchFromGitRepo :: FilePath -> FilePath -> String -> IO (Source Char)
fetchFromGitRepo repoPath path sha = join $ withRepository lgFactory repoPath $ do
    object <- unTagged <$> parseObjOid (T.pack sha)
    commitIHope <- lookupObject object
    commit <- case commitIHope of
      (CommitObj commit) -> return commit
      obj -> error "Expected commit SHA"
    tree <- lookupTree (commitTree commit)
    entry <- treeEntry tree (B1.pack path)
    bytestring <- case entry of
                 Nothing -> return mempty
                 Just BlobEntry {..} -> do
                   blob <- lookupBlob blobEntryOid
                   let (BlobString s) = blobContents blob
                   return s
    return $ transcode bytestring

-- | Return a renderer from the command-line arguments that will print the diff.
printDiff :: Arguments -> FilePath -> Renderer T.Text (IO ())
printDiff arguments filepath diff sources = case format arguments of
  Unified -> B1.putStr =<< unified diff sources
  Split -> put (output arguments) =<< split diff sources
    where
      put Nothing rendered = TextIO.putStr rendered
      put (Just path) rendered = do
        isDir <- doesDirectoryExist path
        let outputPath = if isDir
                         then path </> (takeFileName filepath -<.> ".html")
                         else path
        IO.withFile outputPath IO.WriteMode (flip TextIO.hPutStr rendered)
  Patch -> putStr $ PatchOutput.patch diff sources
