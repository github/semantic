module DiffOutput where

import Source
import Term
import Control.Comonad.Cofree
import qualified Data.Text as T
import Diff
import Syntax
import Range
import Renderer
import qualified Data.ByteString.Char8 as B1
import qualified Data.Text.ICU.Detect as Detect
import qualified Data.Text.ICU.Convert as Convert
import Split
import Unified
import System.Directory
import System.FilePath
import qualified System.IO as IO
import qualified Data.Text.Lazy.IO as TextIO
import qualified PatchOutput
import qualified Parsers as P
import Rainbow

-- | The available types of diff rendering.
data Format = Unified | Split | Patch

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }

parserForFilepath :: FilePath -> P.Parser
parserForFilepath = P.parserForType . T.pack . takeExtension

-- | Replace every string leaf with leaves of the words in the string.
breakDownLeavesByWord :: Source Char -> Term T.Text Info -> Term T.Text Info
breakDownLeavesByWord source = cata replaceIn
  where
    replaceIn info@(Info range categories) (Leaf _) | ranges <- rangesAndWordsInSource range, length ranges > 1 = info :< (Indexed $ makeLeaf categories <$> ranges)
    replaceIn info syntax = info :< syntax
    rangesAndWordsInSource range = rangesAndWordsFrom (start range) (Source.toList $ slice range source)
    makeLeaf categories (range, substring) = Info range categories :< Leaf (T.pack substring)

-- | Transcode a file to a unicode source.
transcode :: B1.ByteString -> IO (Source Char)
transcode text = fromText <$> do
  match <- Detect.detectCharset text
  converter <- Convert.open match Nothing
  return $ Convert.toUnicode converter text

-- | Read the file and convert it to Unicode.
readAndTranscodeFile :: FilePath -> IO (Source Char)
readAndTranscodeFile path = do
  text <- B1.readFile path
  transcode text

-- | Return a renderer from the command-line arguments that will print the diff.
printDiff :: DiffArguments -> Renderer T.Text (IO ())
printDiff arguments diff sources = case format arguments of
  Unified -> put $ unified diff sources
    where
      put chunks = do
        renderer <- byteStringMakerFromEnvironment
        B1.putStr $ mconcat $ chunksToByteStrings renderer chunks
  Split -> put (output arguments) $ split diff sources
    where
      put Nothing rendered = TextIO.putStr rendered
      put (Just path) rendered = do
        isDir <- doesDirectoryExist path
        let outputPath = if isDir
                         then path </> (takeFileName outputPath -<.> ".html")
                         else path
        IO.withFile outputPath IO.WriteMode (flip TextIO.hPutStr rendered)
  Patch -> putStr $ PatchOutput.patch diff sources
