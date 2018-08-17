module Data.Reprinting.Splice where

import Data.Sequence (singleton)
import Data.String
import Prologue

-- | The simplest possible representation of concrete syntax: either
-- it's a run of literal text or information about whitespace.
data Splice
  = Insert Text
  | Directive Layout
    deriving (Eq, Show)

splice :: Text -> Seq Splice
splice = singleton . Insert

-- | Indentation/spacing directives.
data Layout
  = HardWrap Int Indent
  | SoftWrap
  | Don't
    deriving (Eq, Show)

-- | Indentation types. This will eventually be moved into the rules engine.
data Indent = Space | Tab deriving (Eq, Show)


instance IsString Splice where fromString = Insert . fromString
