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
  { sourceFilePath      :: Input
  , format              :: Format
  }
  } deriving (Read)

parseAST :: Parser SemanticAST
parseAST = SemanticAST
      <$> option auto
          ( long "sourcefile"
         <> metavar "FILEPATH"
         <> help "Specify filepath containing source code to parse" )
      <*> option auto
          ( long "format"
         <> help "Specify desired output: show, json, sexpression" )

main :: IO ()
main = generateAST =<< execParser opts

generateAST :: SemanticAST -> IO ()
generateAST (SemanticAST sourceInput _) = do
  case sourceInput of
    FileInput filePath -> do
      bytestring <- Data.ByteString.readFile filePath
      print =<< parseByteString @TreeSitter.Python.AST.Module @(Range, Span) tree_sitter_python bytestring
    StdInput -> do
      bytestring <- Data.ByteString.Char8.pack . Prelude.head <$> getArgs
      print =<< parseByteString @TreeSitter.Python.AST.Module @(Range, Span) tree_sitter_python bytestring

opts :: ParserInfo SemanticAST
opts = info (parseAST <**> helper)
  ( fullDesc
 <> progDesc "Parse source code and produce an AST"
 <> header "semantic-ast is a package used to parse source code" )

-- TODO: Define formats for json, sexpression, etc.
data Format = Show
  deriving (Read)

data Input
  = FileInput FilePath
  | StdInput
  deriving (Read)

input :: Parser Input
input = fileInput <|> stdInput

-- pass in a file as an argument and parse its contents
fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

-- Read from stdin
stdInput :: Parser Input
stdInput = flag' StdInput
  (  long "stdin"
  <> help "Read from stdin" )
