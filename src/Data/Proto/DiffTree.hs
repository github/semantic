{-# LANGUAGE DeriveAnyClass #-}
module Data.Proto.DiffTree (DiffTree(..), ResponseType(..)) where

import Prologue

import           Data.Language
import           Data.Diff
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

type GoDiff = Diff (Sum Go.Syntax) () ()
type HaskellDiff = Diff (Sum Haskell.Syntax) () ()
type JavaDiff = Diff (Sum Java.Syntax) () ()
type JSONDiff = Diff (Sum JSON.Syntax) () ()
type MarkdownDiff = Diff (Sum Markdown.Syntax) () ()
type PythonDiff = Diff (Sum Python.Syntax) () ()
type RubyDiff = Diff (Sum Ruby.Syntax) () ()
type TypeScriptDiff = Diff (Sum TypeScript.Syntax) () ()
type PHPDiff = Diff (Sum PHP.Syntax) () ()

data DiffTree
  = DiffTree
  { languageBefore :: Language
  , languageAfter :: Language
  , pathBefore :: FilePath
  , pathAfter :: FilePath
  , responseType :: Maybe ResponseType
  } deriving (Eq, Show, Generic, Named)

data ResponseType
  = ParseDiffError String
  | GoDiffResponse GoDiff
  | HaskellDiffResponse HaskellDiff
  | JavaDiffResponse JavaDiff
  | JSONDiffResponse JSONDiff
  | MarkdownDiffResponse MarkdownDiff
  | PythonDiffResponse PythonDiff
  | RubyDiffResponse RubyDiff
  | TypeScriptDiffResponse TypeScriptDiff
  | PHPDiffResponse PHPDiff
  deriving (Eq, Show)


-- Instances

instance Message DiffTree where
  encodeMessage _ DiffTree{..}
    =  encodeMessageField 1 languageBefore
    <> encodeMessageField 2 languageAfter
    <> encodeMessageField 3 pathBefore
    <> encodeMessageField 4 pathAfter
    <> case responseType of
         Just (ParseDiffError x)         -> Encode.embedded 5 (encodeMessageField 1 x)
         Just (GoDiffResponse x)         -> Encode.embedded 6 (encodeMessage 1 x)
         Just (HaskellDiffResponse x)    -> Encode.embedded 7 (encodeMessage 1 x)
         Just (JavaDiffResponse x)       -> Encode.embedded 8 (encodeMessage 1 x)
         Just (JSONDiffResponse x)       -> Encode.embedded 9 (encodeMessage 1 x)
         Just (MarkdownDiffResponse x)   -> Encode.embedded 10 (encodeMessage 1 x)
         Just (PythonDiffResponse x)     -> Encode.embedded 11 (encodeMessage 1 x)
         Just (RubyDiffResponse x)       -> Encode.embedded 12 (encodeMessage 1 x)
         Just (TypeScriptDiffResponse x) -> Encode.embedded 13 (encodeMessage 1 x)
         Just (PHPDiffResponse x)        -> Encode.embedded 14 (encodeMessage 1 x)
         _ -> mempty
  decodeMessage = error "decodeMessage not implemented for DiffTree"
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single "Language") (Single "language_before") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim . Named $ Single "Language") (Single "language_after") [] Nothing
    , DotProtoMessageField $ DotProtoField 3 (Prim PB.String) (Single "path_before") [] Nothing
    , DotProtoMessageField $ DotProtoField 4 (Prim PB.String) (Single "path_after") [] Nothing
    , DotProtoMessageOneOf (Single "response_type")
      [ DotProtoField 5 (Prim PB.String) (Single "error") [] Nothing
      , DotProtoField 6 (Prim . Named $ Dots (Path ["godiff", "GoDiff"])) (Single "go_diff") [] Nothing
      , DotProtoField 7 (Prim . Named $ Dots (Path ["haskelldiff", "HaskellDiff"])) (Single "haskell_diff") [] Nothing
      , DotProtoField 8 (Prim . Named $ Dots (Path ["javadiff", "JavaDiff"])) (Single "java_diff") [] Nothing
      , DotProtoField 9 (Prim . Named $ Dots (Path ["jsondiff", "JSONDiff"])) (Single "json_diff") [] Nothing
      , DotProtoField 10 (Prim . Named $ Dots (Path ["markdowndiff", "MarkdownDiff"])) (Single "markdown_diff") [] Nothing
      , DotProtoField 11 (Prim . Named $ Dots (Path ["pythondiff", "PythonDiff"])) (Single "python_diff") [] Nothing
      , DotProtoField 12 (Prim . Named $ Dots (Path ["rubydiff", "RubyDiff"])) (Single "ruby_diff") [] Nothing
      , DotProtoField 13 (Prim . Named $ Dots (Path ["typescriptdiff", "TypeScriptDiff"])) (Single "typescript_diff") [] Nothing
      , DotProtoField 14 (Prim . Named $ Dots (Path ["phpdiff", "PHPDiff"])) (Single "php_diff") [] Nothing
      ]
    ]
