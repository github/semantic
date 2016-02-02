{-# LANGUAGE RecordWildCards #-}
 module Main where

import Categorizable
import Diff
import Interpreter
import qualified Parsers as P
import Syntax
import Range
import qualified PatchOutput
import Split
import Term
import Unified
import Source
import Control.Comonad.Cofree
import qualified Data.ByteString.Char8 as B1
import Options.Applicative
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TextIO
import qualified System.IO as IO
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert
import Data.Bifunctor.Join
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
  sources <- sequence $ DO.readAndTranscodeFile <$> Join (sourceAPath, sourceBPath)
  let parse = DO.parserForFilepath sourceAPath
  terms <- sequence $ parse <$> sources
  let replaceLeaves = DO.breakDownLeavesByWord <$> sources
  DO.printDiff (args arguments) (uncurry diffTerms $ runJoin $ replaceLeaves <*> terms) (runJoin sources)
  where opts = info (helper <*> arguments)
          (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")
        args Arguments{..} = DO.DiffArguments { format = format, output = output, outputPath = sourceA }
