module Data.Reprinting.Splice
  ( Splice(..)
  , Layout(..)
  , copy
  , splice
  , directive
  , directives
  ) where

import Data.Reprinting.Token
import Data.Sequence (singleton, fromList)
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

-- | Construct multiple layout directives.
directives :: [Layout] -> Seq Splice
directives = fromList . fmap Directive

-- | Indentation/spacing directives.
data Layout
  = HardWrap
  | SoftWrap
  | Space
  | Indent
    deriving (Eq, Show)
