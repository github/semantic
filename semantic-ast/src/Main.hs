{-# LANGUAGE TypeApplications #-}
module Main (main) where

import System.Environment
import TreeSitter.Unmarshal
import qualified TreeSitter.Python.AST as AST
import qualified TreeSitter.Python as Python
import Source.Range
import Source.Span
import Data.ByteString.Char8
import Data.ByteString (pack, readFile, ByteString)
import System.IO (FilePath)
import Options.Applicative hiding (style)
import Data.Semigroup ((<>))
import Text.Pretty.Simple (pPrint, pPrintNoColor)
import Data.Foldable (traverse_)

data SemanticAST = SemanticAST
  { format :: Format
  , color  :: Bool
  , source :: Either [FilePath] String
  }

-- Usage: semantic-ast --format ARG [--no-color] (--sourceString STRING | FILEPATHS…)
parseAST :: Parser SemanticAST
parseAST = SemanticAST
  <$> option auto
    ( long "format"
    <> help "Specify desired output: show, json, sexpression" )
  <*> switch
    ( long "no-color"
    <> help "Print with color: --color"
    )
  <*> (Left <$> some
    (Options.Applicative.argument str (metavar "FILEPATH(S)"))
    <|> Right <$> strOption
      ( long "sourceString"
      <> metavar "STRING"
      <> help "Specify source input to parse"
      ))


main :: IO ()
main = generateAST =<< execParser opts


generateAST :: SemanticAST -> IO ()
generateAST (SemanticAST format color source) =
  getByteStrings >>= traverse_ go
  where getByteStrings = case source of
          Left filePaths -> traverse Data.ByteString.readFile filePaths
          Right source   -> pure [Data.ByteString.Char8.pack source]
        go bytestring = do
          ast <- parseByteString @AST.Module @(Range, Span) Python.tree_sitter_python bytestring
          case format of
            Show   -> print ast
            Pretty -> case color of
              True   -> pPrintNoColor ast
              False  -> pPrint ast


opts :: ParserInfo SemanticAST
opts = info (parseAST <**> helper)
  ( fullDesc
 <> progDesc "Parse source code and produce an AST"
 <> header "semantic-ast is a package used to parse source code" )

-- TODO: Define formats for json, sexpression, etc.
data Format = Show | Pretty | JSON
  deriving (Read)
