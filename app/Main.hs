{-# LANGUAGE RecordWildCards #-}
 module Main where

import Source
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
  let (sourceAPath, sourceBPath) = (sourceA arguments, sourceB arguments)
  sources <- sequence $ readAndTranscodeFile <$> Join (sourceAPath, sourceBPath)
  let srcs = runJoin sources
  let sourceBlobs = (SourceBlob (fst srcs) mempty sourceAPath, SourceBlob (snd srcs) mempty sourceBPath)
  DO.printDiff (parserForFilepath $ sourceA arguments) (args arguments) sourceBlobs
  where opts = info (helper <*> arguments)
          (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")
        args Arguments{..} = DO.DiffArguments { format = format, output = output, outputPath = sourceA }
