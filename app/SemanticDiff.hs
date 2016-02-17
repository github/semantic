{-# LANGUAGE RecordWildCards #-}
module Main where

import Diffing
import Source
import Options.Applicative
import qualified Data.ByteString.Char8 as B1
import qualified Data.Text as T
import Data.Bifunctor.Join
import Git.Libgit2
import Git.Types
import Git.Repository
import Data.Tagged
import Control.Monad.Reader
import System.Environment
import qualified DiffOutput as DO

-- | The command line arguments to the application.
data Arguments = Arguments { format :: DO.Format, output :: Maybe FilePath, shaA :: String, shaB :: String, filepaths :: [FilePath] }

-- | A parser for the application's command-line arguments.
arguments :: Parser Arguments
arguments = Arguments
  <$> (flag DO.Split DO.Unified (long "unified" <> help "output a unified diff")
  <|> flag DO.Split DO.Patch (long "patch" <> help "output a patch(1)-compatible diff")
  <|> flag' DO.Split (long "split" <> help "output a split diff"))
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
    DO.printDiff (parserForFilepath filepath) (args arguments filepath) (runJoin sources)
    where opts = info (helper <*> arguments)
            (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")
          args Arguments{..} filepath = DO.DiffArguments { format = format, output = output, outputPath = filepath }

-- | Returns a file source given an absolute repo path, a relative file path, and the sha to look up.
fetchFromGitRepo :: FilePath -> FilePath -> String -> IO (Source Char)
fetchFromGitRepo repoPath path sha = join $ withRepository lgFactory repoPath $ do
    object <- unTagged <$> parseObjOid (T.pack sha)
    commitIHope <- lookupObject object
    commit <- case commitIHope of
      (CommitObj commit) -> return commit
      _ -> error "Expected commit SHA"
    tree <- lookupTree (commitTree commit)
    entry <- treeEntry tree (B1.pack path)
    bytestring <- case entry of
                 Nothing -> return mempty
                 Just BlobEntry {..} -> do
                   blob <- lookupBlob blobEntryOid
                   let (BlobString s) = blobContents blob
                   return s
    return $ transcode bytestring
