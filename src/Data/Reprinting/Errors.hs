module Data.Reprinting.Errors ( TranslationError (..) ) where

import Data.Reprinting.Token
import Data.Reprinting.Scope

-- | Represents failure occurring in a 'Concrete' machine during the translation
-- phases of the reprinting pipeline.
data TranslationError
  = UnbalancedPair Scope [Scope]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | NoTranslation Element [Scope]
  -- ^ Thrown if no translation found for a given element.
    deriving (Eq, Show)
