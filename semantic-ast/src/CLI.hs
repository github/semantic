{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
module CLI (main) where

import System.Environment
import TreeSitter.Unmarshal
import TreeSitter.Python.AST
import TreeSitter.Python
import Source.Range
import Source.Span
import Data.ByteString (readFile, ByteString)
import System.IO (FilePath)
import Options.Applicative hiding (style)
import Data.Semigroup ((<>))

data SemanticAST = SemanticAST
  { sourceFilePath :: Prelude.String
  , format         :: Prelude.String
  }

parseAST :: Parser SemanticAST
parseAST = SemanticAST
    <$> strOption
        ( long "semantic-ast"
       <> metavar "FILEPATH"
       <> help "Specify filepath containing source code to parse" )
    <*> strOption
        ( long "format"
       <> help "Specify format --json --sexpression --show" )

main :: IO ()
main = do
  args <- head <$> getArgs
  bytestring <- Data.ByteString.readFile args
  print =<< parseByteString @TreeSitter.Python.AST.Module @(Range, Span) tree_sitter_python bytestring
