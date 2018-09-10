{-# LANGUAGE DeriveAnyClass #-}
module Data.ParseTree (ParseTree(..), ResponseType(..)) where

import Prologue
import Data.Language
import Data.Term
import qualified Language.Java.Assignment as Java
import qualified Language.Ruby.Assignment as Ruby
import Proto3.Suite
import qualified Proto3.Wire.Encode as Encode
import qualified Proto3.Suite as PB

type JavaTerm = Term (Sum Java.Syntax) ()
type RubyTerm = Term (Sum Ruby.Syntax) ()

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
         Just (JavaResponse x) -> Encode.embedded 4 (encodeMessage 1 x)
         Just (RubyResponse x) -> Encode.embedded 5 (encodeMessage 1 x)
         _ -> mempty
  decodeMessage = undefined
  dotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim . Named $ Single "Language") (Single "language") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "path") [] Nothing
    , DotProtoMessageField $ DotProtoField 3 (Prim PB.String) (Single "error") [] Nothing
    , DotProtoMessageOneOf (Single "response_type")
      [ DotProtoField 4 (Prim . Named $ Dots (Path ["javaterm", "JavaTerm"])) (Single "java_tree") [] Nothing
      , DotProtoField 5 (Prim . Named $ Dots (Path ["rubyterm", "RubyTerm"])) (Single "ruby_tree") [] Nothing
      ]
    ]

data ResponseType
  = JavaResponse JavaTerm
  | RubyResponse RubyTerm
  deriving (Eq, Show)
