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

data Renderer = Unified | Split | Patch

data Argument = Argument { renderer :: Renderer, output :: Maybe FilePath, sourceA :: FilePath, sourceB :: FilePath }

arguments :: Parser Argument
arguments = Argument
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
  aContents <- readAndTranscodeFile sourceAPath
  bContents <- readAndTranscodeFile sourceBPath
  (aTerm, bTerm) <- let parse = (P.parserForType . T.pack . takeExtension) sourceAPath in do
    aTerm <- parse aContents
    bTerm <- parse bContents
    return (replaceLeavesWithWordBranches aContents aTerm, replaceLeavesWithWordBranches bContents bTerm)
  let diff = interpret comparable aTerm bTerm in
    case renderer arguments of
      Unified -> do
        rendered <- unified diff aContents bContents
        B1.putStr rendered
      Split -> do
        rendered <- split diff aContents bContents
        case output arguments of
          Just path -> do
            isDir <- doesDirectoryExist path
            IO.withFile (if isDir then path </> (takeFileName sourceBPath -<.> ".html") else path) IO.WriteMode (write rendered)
          Nothing -> TextIO.putStr rendered
      Patch -> do
        putStr $ PatchOutput.patch diff aContents bContents
    where
    opts = info (helper <*> arguments)
      (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")
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
