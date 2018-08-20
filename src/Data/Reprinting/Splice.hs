module Data.Reprinting.Splice
  ( Splice(..)
  , Layout(..)
  , Indent(..)
  , copy
  , splice
  , directive
  ) where

import Data.Reprinting.Token
import Data.Sequence (singleton)
import Prologue hiding (Element)

-- | The final representation of concrete syntax in the reprinting pipeline.
-- 'Inserts' have access to the original 'Element' and 'Context' for ease of
-- writing additional steps in the reprinting pipeline.
data Splice
  = Insert Element [Context] Text -- ^ New 'Text' to be inserted, along with original 'Element' and `Context`.
  | Original Text
  | Directive Layout
    deriving (Eq, Show)

-- | Copy in some original, un-refactored 'Text'.
copy :: Text -> Seq Splice
copy = singleton . Original

-- | Construct a splice to insert.
splice :: Element -> [Context] -> Text -> Seq Splice
splice el c = singleton . Insert el c

-- | Construct a layout directive.
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
