module Data.Abstract.Module
( Module(..)
, ModuleName
, moduleForBlob
) where

import Data.Abstract.FreeVariables
import Data.Blob
import qualified Data.ByteString.Char8 as BC
import Data.Language
import Data.List.Split (splitWhen)
import System.FilePath.Posix

type ModuleName = Name

data Module term = Module { moduleName :: ModuleName, modulePath :: FilePath, moduleBody :: term }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


moduleForBlob :: FilePath -> Blob -> term -> Module term
moduleForBlob rootDir blob term = Module (moduleName blob) (blobPath blob) term
  where moduleName Blob{..} = let path = dropExtensions (makeRelative rootDir blobPath)
         in case blobLanguage of
          -- TODO: Need a better way to handle module registration and resolution
          Just Go -> toName (takeDirectory path) -- Go allows defining modules across multiple files in the same directory.
          _ ->  toName path
        toName str = qualifiedName (fmap BC.pack (splitWhen (== pathSeparator) str))
