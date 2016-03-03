module DiffOutput where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.IO as TextIO
import Data.Functor.Both
import Diffing
import Parser
import qualified Renderer.JSON as J
import qualified Renderer.Patch as P
import Renderer.Split
import Source
import System.Directory
import System.FilePath
import qualified System.IO as IO

-- | The available types of diff rendering.
data Format = Split | Patch | JSON

data DiffArguments = DiffArguments { format :: Format, output :: Maybe FilePath, outputPath :: FilePath }

-- | Return a renderer from the command-line arguments that will print the diff.
printDiff :: Parser -> DiffArguments -> Both SourceBlob -> IO ()
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
  Patch -> putStrLn =<< diffFiles parser P.patch sources
  JSON -> B.putStr =<< diffFiles parser J.json sources
