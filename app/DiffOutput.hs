module DiffOutput where

import Diffing
import qualified Data.ByteString.Char8 as B1
import Parser
import Source
import Unified
import System.Directory
import System.FilePath
import qualified System.IO as IO
import qualified Data.Text.Lazy.IO as TextIO
import qualified Renderer.Patch as P
import Renderer.Split
import Rainbow

-- | The available types of diff rendering.
data Format = Unified | Split | Patch

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }

-- | Return a renderer from the command-line arguments that will print the diff.
printDiff :: Parser -> DiffArguments -> (SourceBlob, SourceBlob) -> IO ()
printDiff parser arguments sources = case format arguments of
  Unified -> put =<< diffFiles parser unified sources
    where
      put chunks = do
        renderer <- byteStringMakerFromEnvironment
        B1.putStr $ mconcat $ chunksToByteStrings renderer chunks
  Split -> put (output arguments) =<< diffFiles parser split sources
    where
      put Nothing rendered = TextIO.putStr rendered
      put (Just path) rendered = do
        isDir <- doesDirectoryExist path
        let outputPath = if isDir
                         then path </> (takeFileName outputPath -<.> ".html")
                         else path
        IO.withFile outputPath IO.WriteMode (`TextIO.hPutStr` rendered)
  Patch -> putStr =<< diffFiles parser P.patch sources
