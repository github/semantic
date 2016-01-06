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
import Data.Biapplicative
import Data.Bifunctor.Join

data Renderer = Unified | Split | Patch

data Arguments = Arguments { renderer :: Renderer, output :: Maybe FilePath, sourceA :: FilePath, sourceB :: FilePath }

arguments :: Parser Arguments
arguments = Arguments
  <$> (flag Split Unified (long "unified" <> help "output a unified diff")
  <|> flag Split Patch (long "patch" <> help "output a patch(1)-compatible diff")
  <|> flag' Split (long "split" <> help "output a split diff"))
  <*> (optional $ strOption (long "output" <> short 'o' <> help "output directory for split diffs, defaulting to stdout if unspecified"))
  <*> strArgument (metavar "FILE a")
  <*> strArgument (metavar "FILE b")

main :: IO ()
main = do
  arguments <- execParser opts
  let (sourceAPath, sourceBPath) = (sourceA arguments, sourceB arguments)
  sources <- sequence $ readAndTranscodeFile <$> Join (sourceAPath, sourceBPath)
  let parse = (P.parserForType . T.pack . takeExtension) sourceAPath
  terms <- sequence $ parse <$> sources
  let replaceLeaves = replaceLeavesWithWordBranches <$> sources
  printDiff arguments (runJoin sources) (runJoin $ replaceLeaves <*> terms)
  where opts = info (helper <*> arguments)
          (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")

printDiff :: Arguments -> (Source Char, Source Char) -> (Term T.Text Info, Term T.Text Info) -> IO ()
printDiff arguments (aSource, bSource) (aTerm, bTerm) = case renderer arguments of
  Unified -> do
    rendered <- unified diff aSource bSource
    B1.putStr rendered
  Split -> do
    rendered <- split diff aSource bSource
    case output arguments of
      Just path -> do
        isDir <- doesDirectoryExist path
        let outputPath = if isDir
                         then path </> (takeFileName (sourceB arguments) -<.> ".html")
                         else path
        IO.withFile outputPath IO.WriteMode (write rendered)
      Nothing -> TextIO.putStr rendered
  Patch -> do
    putStr $ PatchOutput.patch diff aSource bSource
  where diff = interpret comparable aTerm bTerm
        write rendered h = TextIO.hPutStr h rendered

replaceLeavesWithWordBranches :: Source Char -> Term T.Text Info -> Term T.Text Info
replaceLeavesWithWordBranches source = replaceIn source 0
  where
    replaceIn source startIndex (info@(Info range categories) :< syntax) | substring <- slice (offsetRange (negate startIndex) range) source = info :< case syntax of
      Leaf _ | ranges <- rangesAndWordsFrom (start range) (toList substring), length ranges > 1 -> Indexed $ makeLeaf categories <$> ranges
      Indexed i -> Indexed $ replaceIn substring (start range) <$> i
      Fixed f -> Fixed $ replaceIn substring (start range) <$> f
      Keyed k -> Keyed $ replaceIn substring (start range) <$> k
      _ -> syntax
    makeLeaf categories (range, substring) = Info range categories :< Leaf (T.pack substring)

readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = fromText <$> do
  text <- B1.readFile path
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  return $ Convert.toUnicode converter text
