module Data.Language
  ( module Source.Language
  , LanguageMode(..)
  , PerLanguageModes(..)
  , defaultLanguageModes
  , preciseLanguageModes
  , aLaCarteLanguageModes
  ) where

import qualified Data.Languages as Lingo
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Source.Language

data PerLanguageModes = PerLanguageModes
  { pythonMode     :: LanguageMode
  , rubyMode       :: LanguageMode
  , goMode         :: LanguageMode
  , typescriptMode :: LanguageMode
  , tsxMode        :: LanguageMode
  , javascriptMode :: LanguageMode
  , jsxMode        :: LanguageMode
  }
  deriving (Eq, Ord, Show)

defaultLanguageModes :: PerLanguageModes
defaultLanguageModes = preciseLanguageModes

aLaCarteLanguageModes :: PerLanguageModes
aLaCarteLanguageModes = PerLanguageModes
  { pythonMode = ALaCarte
  , rubyMode = ALaCarte
  , goMode = ALaCarte
  , typescriptMode = ALaCarte
  , tsxMode = ALaCarte
  , javascriptMode = ALaCarte
  , jsxMode = ALaCarte
  }

preciseLanguageModes :: PerLanguageModes
preciseLanguageModes = PerLanguageModes
  { pythonMode = Precise
  , rubyMode = Precise
  , goMode = Precise
  , typescriptMode = Precise
  , tsxMode = Precise
  , javascriptMode = Precise
  , jsxMode = Precise
  }

data LanguageMode
  = ALaCarte
  | Precise
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
