module Data.Reprinting.Splice where

import Data.Sequence (singleton)
import Data.String
import Prologue hiding (Element)
import Data.Sequence
import Data.Reprinting.Token

-- | The simplest possible representation of concrete syntax: either
-- it's a run of literal text or information about whitespace.
data Splice
  = Insert Element (Maybe Context) Text
  | Original Text
  | Directive Layout
    deriving (Eq, Show)

copy :: Text -> Seq Splice
copy = singleton . Original

splice :: Element -> Maybe Context -> Text -> Seq Splice
splice el c = singleton . Insert el c

directive :: Layout -> Seq Splice
directive = singleton . Directive

-- | Indentation/spacing directives.
data Layout
  = HardWrap Int Indent
  | SoftWrap
  | Don't
    deriving (Eq, Show)

-- | Indentation types. This will eventually be moved into the rules engine.
data Indent = Space | Tab deriving (Eq, Show)


-- instance IsString Splice where fromString = Insert . fromString
