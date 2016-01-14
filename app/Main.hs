module Main where

import Categorizable
import Diff
import Interpreter
import qualified Parsers as P
import Syntax
import Range
import qualified PatchOutput
import Renderer
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

-- | The available types of diff rendering.
data Format = Unified | Split | Patch

-- | The command line arguments to the application.
data Arguments = Arguments { format :: Format, output :: Maybe FilePath, sourceA :: FilePath, sourceB :: FilePath }

-- | A parser for the application's command-line arguments.
arguments :: Parser Arguments
arguments = Arguments
  <$> (flag Split Unified (long "unified" <> help "output a unified diff")
  <|> flag Split Patch (long "patch" <> help "output a patch(1)-compatible diff")
  <|> flag' Split (long "split" <> help "output a split diff"))
  <*> optional (strOption (long "output" <> short 'o' <> help "output directory for split diffs, defaulting to stdout if unspecified"))
  <*> strArgument (metavar "FILE a")
  <*> strArgument (metavar "FILE b")

main :: IO ()
main = do
  arguments <- execParser opts
  let (sourceAPath, sourceBPath) = (sourceA arguments, sourceB arguments)
  sources <- sequence $ readAndTranscodeFile <$> Join (sourceAPath, sourceBPath)
  let parse = (P.parserForType . T.pack . takeExtension) sourceAPath
  terms <- sequence $ parse <$> sources
  let replaceLeaves = breakDownLeavesByWord <$> sources
  printDiff arguments (uncurry diff $ runJoin $ replaceLeaves <*> terms) (runJoin sources)
  where opts = info (helper <*> arguments)
          (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")

-- | Diff two terms.
diff :: (Eq a, Eq annotation, Categorizable annotation) => Term a annotation -> Term a annotation -> Diff a annotation
diff = interpret comparable

-- | Return a renderer from the command-line arguments that will print the diff.
printDiff :: Arguments -> Renderer T.Text (IO ())
printDiff arguments diff sources = case format arguments of
  Unified -> B1.putStr =<< unified diff sources
  Split -> put (output arguments) =<< split diff sources
    where
      put Nothing rendered = TextIO.putStr rendered
      put (Just path) rendered = do
        isDir <- doesDirectoryExist path
        let outputPath = if isDir
                         then path </> (takeFileName (sourceB arguments) -<.> ".html")
                         else path
        IO.withFile outputPath IO.WriteMode (flip TextIO.hPutStr rendered)
  Patch -> putStr $ PatchOutput.patch diff sources

-- | Replace every string leaf with leaves of the words in the string.
breakDownLeavesByWord :: Source Char -> Term T.Text Info -> Term T.Text Info
breakDownLeavesByWord source = cata replaceIn
  where
    replaceIn info@(Info range categories) (Leaf _) | ranges <- rangesAndWordsInSource range, length ranges > 1 = info :< (Indexed $ makeLeaf categories <$> ranges)
    replaceIn info syntax = info :< syntax
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (toList $ slice range source)
    makeLeaf categories (range, substring) = Info range categories :< Leaf (T.pack substring)

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = fromText <$> do
  text <- B1.readFile path
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  return $ Convert.toUnicode converter text
