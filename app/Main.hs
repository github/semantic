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
import qualified System.IO as IO

data Renderer = Unified | Split

data Argument = Argument { renderer :: Renderer, output :: Maybe FilePath, sourceA :: FilePath, sourceB :: FilePath }

arguments :: Parser Argument
arguments = Argument
  <$> (flag Split Unified (long "unified" <> help "output a unified diff")
  <|> flag' Split (long "split" <> help "output a split diff"))
  <*> option auto (long "output" <> short 'o' <> value Nothing <> help "output directory for split diffs, defaulting to stdout if unspecified")
  <*> strArgument (metavar "FILE a")
  <*> strArgument (metavar "FILE b")

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
    case renderer arguments of
      Unified -> do
        rendered <- unified diff aContents bContents
        B1.putStr rendered
      Split -> do
        rendered <- split diff aContents bContents
        case output arguments of
          Just path -> IO.withFile path IO.WriteMode (write rendered)
          Nothing -> B2.putStr rendered
    where
    opts = info (helper <*> arguments)
      (fullDesc <> progDesc "Diff some things" <> header "semantic-diff - diff semantically")
    write rendered h = B2.hPut h rendered

parserForType :: String -> P.Parser
parserForType mediaType = maybe P.lineByLineParser parseTreeSitterFile $ case mediaType of
    ".h" -> Just ts_language_c
    ".c" -> Just ts_language_c
    ".js" -> Just ts_language_javascript
    _ -> Nothing

replaceLeavesWithWordBranches :: String -> Term String Info -> Term String Info
replaceLeavesWithWordBranches source term = replaceIn source 0 term
  where
    replaceIn source startIndex (info@(Info range categories) :< syntax) | substring <- substring (offsetRange (negate startIndex) range) source = info :< case syntax of
      Leaf _ | ranges <- rangesAndWordsFrom (start range) substring, length ranges > 1 -> Indexed $ makeLeaf substring startIndex categories <$> ranges
      Indexed i -> Indexed $ replaceIn substring (start range) <$> i
      Fixed f -> Fixed $ replaceIn substring (start range) <$> f
      Keyed k -> Keyed $ replaceIn substring (start range) <$> k
      _ -> syntax
    makeLeaf source startIndex categories (range, substring) = Info range categories :< Leaf substring
