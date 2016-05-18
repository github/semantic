module DiffOutput where

import qualified Data.Text.IO as TextIO
import Data.Functor.Both
import Diffing
import Parser
import qualified Renderer.JSON as J
import qualified Renderer.Patch as P
import qualified Renderer.Summary as S
import Renderer
import Renderer.Split
import Source
import System.Directory
import System.FilePath
import qualified System.IO as IO
import Data.Text hiding (split)

-- | Returns a rendered diff given a parser, diff arguments and two source blobs.
textDiff :: Parser -> DiffArguments -> Both SourceBlob -> IO Text
textDiff parser arguments sources = case format arguments of
  Split -> diffFiles parser split sources
  Patch -> diffFiles parser P.patch sources
  JSON -> diffFiles parser J.json sources
  Summary -> diffFiles parser S.summary sources

-- | Returns a truncated diff given diff arguments and two source blobs.
truncatedDiff :: DiffArguments -> Both SourceBlob -> IO Text
truncatedDiff arguments sources = case format arguments of
  Split -> return ""
  Patch -> return $ P.truncatePatch arguments sources
  JSON -> return "{}"
  Summary -> return ""

-- | Prints a rendered diff to stdio or a filepath given a parser, diff arguments and two source blobs.
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
  Patch -> TextIO.putStr =<< diffFiles parser P.patch sources
  JSON -> TextIO.putStr =<< diffFiles parser J.json sources
  Summary -> TextIO.putStr =<< diffFiles parser S.summary sources
