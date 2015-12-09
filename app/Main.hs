module Main where

import Categorizable
import Diff
import Interpreter
import qualified Parser as P
import Syntax
import Range
import Split
import Term
import TreeSitter
import Unified
import Control.Comonad.Cofree
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as B1
import qualified Data.ByteString.Lazy as B2
import Data.Set hiding (split)
import Options.Applicative
import System.FilePath

import Foreign.Ptr

data Output = Unified | Split

data Argument = Argument { output :: Output, sourceA :: FilePath, sourceB :: FilePath }

arguments :: Parser Argument
arguments = Argument
  <$> (flag Split Unified (long "unified" <> help "output a unified diff")
  <|> flag' Split (long "split" <> help "output a split diff"))
  <*> argument str (metavar "FILE a")
  <*> argument str (metavar "FILE b")

main :: IO ()
main = do
  arguments <- execParser opts
  let (sourceAPath, sourceBPath) = (sourceA arguments, sourceB arguments)
  aContents <- readFile sourceAPath
  bContents <- readFile sourceBPath
  (aTerm, bTerm) <- case (parserForType . takeExtension) sourceAPath of
    Just parse -> do aTerm <- parse aContents
                     bTerm <- parse bContents
                     return (aTerm, bTerm)
    Nothing -> error ("Unsupported language extension in path: " ++ sourceAPath)
  let diff = interpret comparable aTerm bTerm in
    case output arguments of
      Unified -> do
        output <- unified diff aContents bContents
        B1.putStr output
      Split -> do
        output <- split diff aContents bContents
        B2.putStr output
    where
    opts = info (helper <*> arguments)
      (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")

parserForType :: String -> Maybe P.Parser
parserForType mediaType = parseTreeSitterFile <$> case mediaType of
    ".h" -> Just ts_language_c
    ".c" -> Just ts_language_c
    ".js" -> Just ts_language_javascript
    _ -> Nothing
