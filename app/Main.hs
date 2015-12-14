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
import qualified Data.ByteString.Char8 as B1
import qualified Data.ByteString.Lazy as B2
import Options.Applicative
import System.FilePath

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
  (aTerm, bTerm) <- let parse = (parserForType . takeExtension) sourceAPath in do
    aTerm <- parse aContents
    bTerm <- parse bContents
    return (replaceLeavesWithWordBranches aContents aTerm, replaceLeavesWithWordBranches bContents bTerm)
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

parserForType :: String -> P.Parser
parserForType mediaType = maybe P.lineByLineParser parseTreeSitterFile $ case mediaType of
    ".h" -> Just ts_language_c
    ".c" -> Just ts_language_c
    ".js" -> Just ts_language_javascript
    _ -> Nothing

replaceLeavesWithWordBranches :: String -> Term String Info -> Term String Info
replaceLeavesWithWordBranches source term = replaceIn source 0 term
  where
    replaceIn source startIndex (info@(Info range lineRange categories) :< syntax) | substring <- substring (offsetRange (negate startIndex) range) source = info :< case syntax of
      Leaf _ | ranges <- rangesOfWordsFrom (start range) substring, length ranges > 1 -> Indexed $ makeLeaf substring startIndex lineRange categories <$> ranges
      Indexed i -> Indexed $ replaceIn substring (start range) <$> i
      Fixed f -> Fixed $ replaceIn substring (start range) <$> f
      Keyed k -> Keyed $ replaceIn substring (start range) <$> k
      _ -> syntax
    makeLeaf source startIndex lineRange categories range = Info range lineRange categories :< Leaf (substring (offsetRange (negate startIndex) range) source)
