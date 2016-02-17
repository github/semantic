{-# LANGUAGE RecordWildCards #-}
 module Main where

import Data.Bifunctor.Join
import Diffing
import Options.Applicative
import qualified DiffOutput as DO

data Arguments = Arguments { format :: DO.Format, output :: Maybe FilePath, sourceA :: FilePath, sourceB :: FilePath }

arguments :: Parser Arguments
arguments = Arguments
  <$> (flag DO.Split DO.Unified (long "unified" <> help "output a unified diff")
  <|> flag DO.Split DO.Patch (long "patch" <> help "output a patch(1)-compatible diff")
  <|> flag' DO.Split (long "split" <> help "output a split diff"))
  <*> optional (strOption (long "output" <> short 'o' <> help "output directory for split diffs, defaulting to stdout if unspecified"))
  <*> strArgument (metavar "FILE a")
  <*> strArgument (metavar "FILE b")

main :: IO ()
main = do
  arguments <- execParser opts
  sources <- sequence $ readAndTranscodeFile <$> Join (sourceA arguments, sourceB arguments)
  DO.printDiff (parserForFilepath $ sourceA arguments) (args arguments) (runJoin sources)
  where opts = info (helper <*> arguments)
          (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")
        args Arguments{..} = DO.DiffArguments { format = format, output = output, outputPath = sourceA }
