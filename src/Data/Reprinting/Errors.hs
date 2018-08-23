module Data.Reprinting.Errors ( TranslationException (..) ) where

import Data.Reprinting.Token

-- | Represents failure occurring in a 'Concrete' machine during the translation
-- phases of the reprinting pipeline.
data TranslationException
  = UnbalancedPair Context [Context]
  -- ^ Thrown if an unbalanced 'Enter'/'Exit' pair is encountered.
  | NoTranslation Element [Context]
  -- ^ Thrown if no translation found for a given element.
    deriving (Eq, Show)
