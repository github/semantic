module Data.Language
  ( module Source.Language
  , LanguageMode(..)
  , PerLanguageModes(..)
  , defaultLanguageModes
  , preciseLanguageModes
  , aLaCarteLanguageModes
  ) where

import Source.Language

data PerLanguageModes = PerLanguageModes
  { pythonMode     :: LanguageMode
  , rubyMode       :: LanguageMode
  , goMode         :: LanguageMode
  , typescriptMode :: LanguageMode
  , tsxMode        :: LanguageMode
  , javascriptMode :: LanguageMode
  , jsxMode        :: LanguageMode
  , rustMode       :: LanguageMode
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
  , rustMode = ALaCarte
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
  , rustMode = Precise
  }

data LanguageMode
  = ALaCarte
  | Precise
  deriving (Bounded, Enum, Eq, Ord, Read, Show)
