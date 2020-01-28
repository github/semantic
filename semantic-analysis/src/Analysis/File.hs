{-# LANGUAGE DeriveTraversable #-}
module Analysis.File
( File(..)
, fileLanguage
, fromBody
, fromPath
) where

import           Data.Maybe (fromJust, listToMaybe)
import           Data.Semilattice.Lower
import           GHC.Stack
import           Source.Language as Language
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

-- | The language of the provided file, as inferred by 'Language.forPath'.
fileLanguage :: File a -> Language
fileLanguage = Language.forPath . filePath

fromPath :: Path.PartClass.AbsRel ar => Path.File ar -> File Language
fromPath p = File (Path.toAbsRel p) lowerBound (Language.forPath p)
