{-# LANGUAGE DeriveAnyClass #-}
module Data.Proto.ParseTree (ParseTree(..), ResponseType(..)) where

import Prologue

import           Data.Language
import           Data.Term
import qualified Language.Go.Assignment as Go
import qualified Language.Haskell.Assignment as Haskell
import qualified Language.Java.Assignment as Java
import qualified Language.JSON.Assignment as JSON
import qualified Language.Markdown.Assignment as Markdown
import qualified Language.PHP.Assignment as PHP
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript
import           Proto3.Suite
import qualified Proto3.Suite as PB
import qualified Proto3.Wire.Encode as Encode

type GoTerm = Term (Sum Go.Syntax) ()
type HaskellTerm = Term (Sum Haskell.Syntax) ()
type JavaTerm = Term (Sum Java.Syntax) ()
type JSONTerm = Term (Sum JSON.Syntax) ()
type MarkdownTerm = Term (Sum Markdown.Syntax) ()
type PythonTerm = Term (Sum Python.Syntax) ()
type RubyTerm = Term (Sum Ruby.Syntax) ()
type TypeScriptTerm = Term (Sum TypeScript.Syntax) ()
type PHPTerm = Term (Sum PHP.Syntax) ()

data ParseTree
  = ParseTree
  { language :: Language
  , path :: FilePath
  , responseType :: Maybe ResponseType
  } deriving (Eq, Show, Generic, Named)

data ResponseType
  = ParseError String
  | GoResponse GoTerm
  | HaskellResponse HaskellTerm
  | JavaResponse JavaTerm
  | JSONResponse JSONTerm
  | MarkdownResponse MarkdownTerm
  | PythonResponse PythonTerm
  | RubyResponse RubyTerm
  | TypeScriptResponse TypeScriptTerm
  | PHPResponse PHPTerm
  deriving (Eq, Show)


-- Instances

instance Message ParseTree where
  encodeMessage _ ParseTree{..}
    =  encodeMessageField 1 language
    <> encodeMessageField 2 path
    <> case responseType of
         Just (ParseError x)         -> Encode.embedded 3 (encodeMessageField 1 x)
         Just (GoResponse x)         -> Encode.embedded 4 (encodeMessage 1 x)
         Just (HaskellResponse x)    -> Encode.embedded 5 (encodeMessage 1 x)
         Just (JavaResponse x)       -> Encode.embedded 6 (encodeMessage 1 x)
         Just (JSONResponse x)       -> Encode.embedded 7 (encodeMessage 1 x)
         Just (MarkdownResponse x)   -> Encode.embedded 8 (encodeMessage 1 x)
         Just (PythonResponse x)     -> Encode.embedded 9 (encodeMessage 1 x)
         Just (RubyResponse x)       -> Encode.embedded 10 (encodeMessage 1 x)
         Just (TypeScriptResponse x) -> Encode.embedded 11 (encodeMessage 1 x)
         Just (PHPResponse x)        -> Encode.embedded 12 (encodeMessage 1 x)
         _ -> mempty
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single "Language") (Single "language") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "path") [] Nothing
    , DotProtoMessageOneOf (Single "response_type")
      [ DotProtoField 3 (Prim PB.String) (Single "error") [] Nothing
      , DotProtoField 4 (Prim . Named $ Dots (Path ["goterm", "GoTerm"])) (Single "go_tree") [] Nothing
      , DotProtoField 5 (Prim . Named $ Dots (Path ["haskellterm", "HaskellTerm"])) (Single "haskell_tree") [] Nothing
      , DotProtoField 6 (Prim . Named $ Dots (Path ["javaterm", "JavaTerm"])) (Single "java_tree") [] Nothing
      , DotProtoField 7 (Prim . Named $ Dots (Path ["jsonterm", "JSONTerm"])) (Single "json_tree") [] Nothing
      , DotProtoField 8 (Prim . Named $ Dots (Path ["markdownterm", "MarkdownTerm"])) (Single "markdown_tree") [] Nothing
      , DotProtoField 9 (Prim . Named $ Dots (Path ["pythonterm", "PythonTerm"])) (Single "python_tree") [] Nothing
      , DotProtoField 10 (Prim . Named $ Dots (Path ["rubyterm", "RubyTerm"])) (Single "ruby_tree") [] Nothing
      , DotProtoField 11 (Prim . Named $ Dots (Path ["typescriptterm", "TypeScriptTerm"])) (Single "typescript_tree") [] Nothing
      , DotProtoField 12 (Prim . Named $ Dots (Path ["phpterm", "PHPTerm"])) (Single "php_tree") [] Nothing
      ]
    ]
