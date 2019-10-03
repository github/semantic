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
main = generateAST =<< execParser opts
 where
   opts = info (parseAST <**> helper)
     ( fullDesc
    <> progDesc "Read a file, output an AST"
    <> header "semantic-ast - generates ASTs" )

generateAST :: SemanticAST -> IO ()
generateAST (SemanticAST file _) = do
  bytestring <- Data.ByteString.readFile file
  print =<< parseByteString @TreeSitter.Python.AST.Module @(Range, Span) tree_sitter_python bytestring
-- pass in a file as an argument and parse its contents
fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

-- cat something in
stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )
