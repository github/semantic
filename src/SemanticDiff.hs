{-# LANGUAGE TemplateHaskell #-}
module SemanticDiff (main) where

import Arguments
import Prologue hiding (fst, snd)
import Data.String
import Data.Functor.Both
import Data.Version (showVersion)
import Text.Regex
import Options.Applicative hiding (action)
import qualified Paths_semantic_diff as Library (version)
import qualified Renderer as R
import Development.GitRev
import DiffCommand
import ParseCommand
import qualified Data.ByteString as B

main :: IO ()
main = do
  args@Arguments{..} <- programArguments =<< execParser argumentsParser
  text <- case runMode of
    Diff -> diff args
    Parse -> parse args
  writeToOutput outputPath text

-- | A parser for the application's command-line arguments.
argumentsParser :: ParserInfo CmdLineOptions
argumentsParser = info (version <*> helper <*> argumentsP)
                       (fullDesc <> progDesc "Set the GIT_DIR environment variable to specify the git repository. Set GIT_ALTERNATE_OBJECT_DIRECTORIES to specify location of alternates."
                                 <> header "semantic-diff - Show semantic changes between commits")
  where
    argumentsP :: Parser CmdLineOptions
    argumentsP = CmdLineOptions
      <$> (flag R.Split R.Patch (long "patch" <> help "output a patch(1)-compatible diff")
      <|> flag R.Split R.JSON (long "json" <> help "output a json diff")
      <|> flag' R.Split (long "split" <> help "output a split diff")
      <|> flag' R.Summary (long "summary" <> help "output a diff summary")
      <|> flag' R.SExpression (long "sexpression" <> help "output an s-expression diff tree")
      <|> flag' R.TOC (long "toc" <> help "output a table of contents diff summary"))
      <*> optional (option auto (long "timeout" <> help "timeout for per-file diffs in seconds, defaults to 7 seconds"))
      <*> optional (strOption (long "output" <> short 'o' <> help "output directory for split diffs, defaults to stdout if unspecified"))
      <*> switch (long "no-index" <> help "compare two paths on the filesystem")
      <*> some (argument (eitherReader parseShasAndFiles) (metavar "SHA_A..SHAB FILES..."))
      <*> switch (long "development" <> short 'd' <> help "set development mode which prevents timeout behavior by default")
      <*> flag Diff Parse (long "parse" <> short 'p' <> help "parses a source file without diffing")
      where
        parseShasAndFiles :: String -> Either String ExtraArg
        parseShasAndFiles s = case matchRegex regex s of
          Just ["", sha2] -> Right . ShaPair $ both Nothing (Just sha2)
          Just [sha1, sha2] -> Right . ShaPair $ Just <$> both sha1 sha2
          _ -> Right $ FileArg s
          where regex = mkRegexWithOpts "([0-9a-f]{40})\\.\\.([0-9a-f]{40})" True False

versionString :: String
versionString = "semantic-diff version " <> showVersion Library.version <> " (" <> $(gitHash) <> ")"

version :: Parser (a -> a)
version = infoOption versionString (long "version" <> short 'V' <> help "output the version of the program")

writeToOutput :: Maybe FilePath -> ByteString -> IO ()
writeToOutput = maybe B.putStr B.writeFile
