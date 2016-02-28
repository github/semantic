module DiffOutput where

import Diffing
import qualified Data.ByteString.Char8 as B1
import Parser
import Source
import System.Directory
import System.FilePath
import qualified System.IO as IO
import qualified Data.Text.Lazy.IO as TextIO
import qualified Renderer.JSON as J
import qualified Renderer.Patch as P
import Renderer.Split

-- | The available types of diff rendering.
data Format = Split | Patch | JSON

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }

-- | Return a renderer from the command-line arguments that will print the diff.
printDiff :: Parser -> DiffArguments -> (SourceBlob, SourceBlob) -> IO ()
printDiff parser arguments sources = case format arguments of
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
  JSON -> putStr =<< diffFiles parser J.json sources
