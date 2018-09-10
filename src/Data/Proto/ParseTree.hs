{-# LANGUAGE DeriveAnyClass #-}
module Data.Proto.ParseTree (ParseTree(..), ResponseType(..)) where

import Prologue
import Data.Language
import Data.Term
import qualified Language.Go.Assignment as Go
import qualified Language.Java.Assignment as Java
import qualified Language.JSON.Assignment as JSON
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript
import Proto3.Suite
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Suite as PB

type GoTerm = Term (Sum Go.Syntax) ()
type JavaTerm = Term (Sum Java.Syntax) ()
type JSONTerm = Term (Sum JSON.Syntax) ()
type PythonTerm = Term (Sum Python.Syntax) ()
type RubyTerm = Term (Sum Ruby.Syntax) ()
type TypeScriptTerm = Term (Sum TypeScript.Syntax) ()

data ParseTree
  = ParseTree
  { language :: Language
  , path :: FilePath
  , error :: String
  , responseType :: Maybe ResponseType
  } deriving (Eq, Show, Generic, Named)

instance Message ParseTree where
  encodeMessage _ ParseTree{..}
    =  encodeMessageField 1 language
    <> encodeMessageField 2 path
    <> encodeMessageField 3 error
    <> case responseType of
         Just (GoResponse x)         -> Encode.embedded 4 (encodeMessage 1 x)
         Just (JavaResponse x)       -> Encode.embedded 5 (encodeMessage 1 x)
         Just (JSONResponse x)       -> Encode.embedded 6 (encodeMessage 1 x)
         Just (PythonResponse x)     -> Encode.embedded 7 (encodeMessage 1 x)
         Just (RubyResponse x)       -> Encode.embedded 8 (encodeMessage 1 x)
         Just (TypeScriptResponse x) -> Encode.embedded 9 (encodeMessage 1 x)
         _ -> mempty
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single "Language") (Single "language") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "path") [] Nothing
    , DotProtoMessageField $ DotProtoField 3 (Prim PB.String) (Single "error") [] Nothing
    , DotProtoMessageOneOf (Single "response_type")
      [ DotProtoField 4 (Prim . Named $ Dots (Path ["goterm", "GoTerm"])) (Single "go_tree") [] Nothing
      , DotProtoField 5 (Prim . Named $ Dots (Path ["javaterm", "JavaTerm"])) (Single "java_tree") [] Nothing
      , DotProtoField 6 (Prim . Named $ Dots (Path ["jsonterm", "JSONTerm"])) (Single "json_tree") [] Nothing
      , DotProtoField 7 (Prim . Named $ Dots (Path ["pythonterm", "PythonTerm"])) (Single "python_tree") [] Nothing
      , DotProtoField 8 (Prim . Named $ Dots (Path ["rubyterm", "RubyTerm"])) (Single "ruby_tree") [] Nothing
      , DotProtoField 9 (Prim . Named $ Dots (Path ["typescriptterm", "TypeScriptTerm"])) (Single "typescript_tree") [] Nothing
      ]
    ]

data ResponseType
  = GoResponse GoTerm
  | JavaResponse JavaTerm
  | JSONResponse JSONTerm
  | PythonResponse PythonTerm
  | RubyResponse RubyTerm
  | TypeScriptResponse TypeScriptTerm
  deriving (Eq, Show)
