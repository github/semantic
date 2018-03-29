module Data.Abstract.Module
( Module(..)
, ModuleName
, moduleForBlob
) where

import Data.Blob
import System.FilePath.Posix

type ModuleName = FilePath

data Module term = Module
  { modulePath :: FilePath      -- ^ Path to this module
  , moduleBody :: term          -- ^ @term@ body of the module
  } deriving (Eq, Foldable, Functor, Ord, Show, Traversable)


-- | Construct a 'Module' for a 'Blob' and @term@, relative to some root 'FilePath'.
moduleForBlob :: Maybe FilePath -- ^ The root directory relative to which the module will be resolved, if any.
              -> Blob           -- ^ The 'Blob' containing the module.
              -> term           -- ^ The @term@ representing the body of the module.
              -> Module term    -- ^ A 'Module' named appropriate for the 'Blob', holding the @term@, and constructed relative to the root 'FilePath', if any.
moduleForBlob rootDir = Module . modulePath . blobPath
  where modulePath = maybe takeFileName makeRelative rootDir
