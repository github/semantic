{-# LANGUAGE DeriveTraversable #-}
module Analysis.File
( -- * Files
  File(..)
  -- * Constructors
, fromBody
, fromPath
  -- * Eliminators
, fileLanguage
) where

import qualified Analysis.Reference as A
import           Data.Maybe (fromJust, listToMaybe)
import           GHC.Stack
import           Source.Language as Language
import           Source.Span

-- Files

data File a = File
  { fileRef  :: !A.Reference
  , fileBody :: !a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


-- Constructors

fromBody :: HasCallStack => a -> File a
fromBody body = File (A.Reference (srcLocFile srcLoc) (spanFromSrcLoc srcLoc)) body where
  srcLoc = snd (fromJust (listToMaybe (getCallStack callStack)))

fromPath :: FilePath -> File Language
fromPath p = File (A.fromPath p) (Language.forPath p)


-- Eliminators

-- | The language of the provided file, as inferred by 'Language.forPath'.
fileLanguage :: File a -> Language
fileLanguage = Language.forPath . A.refPath . fileRef
