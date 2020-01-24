{-# LANGUAGE DeriveTraversable #-}
module Analysis.File
( File(..)
, fileLanguage
, fromBody
, fileForPath
, fileForTypedPath
) where

import           Analysis.Language
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Semilattice.Lower
import           GHC.Stack
import           Source.Span
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

data File a = File
  { filePath :: !Path.AbsRelFile
  , fileSpan :: {-# UNPACK #-} !Span
  , fileBody :: !a
  }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

fromBody :: HasCallStack => a -> File a
fromBody body = File (Path.absRel (srcLocFile srcLoc)) (spanFromSrcLoc srcLoc) body where
  srcLoc = snd (fromJust (listToMaybe (getCallStack callStack)))

fileLanguage :: File a -> Language
fileLanguage = languageForTypedPath . filePath

-- | DEPRECATED: prefer 'fileForTypedPath' if at all possible.
fileForPath :: FilePath -> File Language
fileForPath p = File (Path.absRel p) lowerBound (languageForFilePath p)

-- | DEPRECATED
fileForTypedPath :: Path.PartClass.AbsRel ar => Path.File ar -> File Language
fileForTypedPath p = File (Path.absRel (Path.toString p)) lowerBound (languageForTypedPath p)
