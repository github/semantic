{-# LANGUAGE TypeApplications #-}

module Main (main) where

import AST.Unmarshal
import qualified Language.Python.AST as AST
import qualified Language.Python.Grammar as Python
import Source.Range
import Source.Span
import Data.Aeson (toJSON)
import Data.ByteString.Char8
import Data.ByteString (readFile)
import Options.Applicative hiding (style)
import Text.Pretty.Simple (pPrint, pPrintNoColor)
import Data.Foldable (traverse_)
import Control.Monad ((>=>))
import AST.Marshal.JSON (marshal)
import Data.ByteString.Lazy.Char8 (putStrLn)
import Data.Aeson.Encode.Pretty (encodePretty)

data SemanticAST = SemanticAST
  { _format :: Format
  , _noColor  :: Bool
  , _source :: Either [FilePath] String
  }

-- Usage: semantic-parse --format ARG [--no-color] (--sourceString STRING | FILEPATHS…)
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
generateAST (SemanticAST format noColor source) =
  getByteStrings >>= traverse_ go
  where getByteStrings = case source of
          Left filePaths -> traverse Data.ByteString.readFile filePaths
          Right source   -> pure [Data.ByteString.Char8.pack source]
        go = ast >=> display
        ast = parseByteString @AST.Module @(Range, Span) Python.tree_sitter_python -- TODO: generalize for all languages
        display = case format of
          Json -> Data.ByteString.Lazy.Char8.putStrLn . encodePretty . either toJSON (marshal . fmap (const ())) -- TODO: replacing range and span annotations with () for which there is a ToJSON instance for now, deal with this later
          Show -> print
          Pretty | noColor -> pPrintNoColor
                 | otherwise -> pPrint


opts :: ParserInfo SemanticAST
opts = info (parseAST <**> helper)
  ( fullDesc
 <> progDesc "Parse source code and produce an AST"
 <> header "semantic-parse is a package used to parse source code" )

-- TODO: Define formats for json, sexpression, etc.
data Format = Show
            | Pretty
            | Json
  deriving (Read)
