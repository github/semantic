module Semantic.Git
  ( -- Primary (partial) API for cmd line git
    clone
  , lsTree
  , catFile

  -- Intermediate datatypes
  , TreeEntry(..)
  , ObjectType(..)
  , ObjectMode(..)
  , OID(..)
  ) where

import Control.Monad.IO.Class
import Data.Text as Text
import Shelly hiding (FilePath)
import System.IO (hSetBinaryMode)

-- | git clone --bare
clone :: Text -> FilePath -> IO ()
clone url path = sh $ do
  run_ "git" ["clone", "--bare", url, pack path]

-- | git cat-file -p
catFile :: FilePath -> OID -> IO Text
catFile gitDir (OID oid) = sh $ do
  run "git" [pack ("--git-dir=" <> gitDir), "cat-file", "-p", oid]

-- | git ls-tree -rz
lsTree :: FilePath -> OID -> IO [TreeEntry]
lsTree gitDir (OID sha) = sh $ do
  out <- run "git" [pack ("--git-dir=" <> gitDir), "ls-tree", "-rz", sha]
  pure $ mkEntry <$> splitOn "\NUL" out
  where
    mkEntry row | [mode, ty, rest] <- splitOn " " row
                , [oid, path] <- splitOn "\t" rest
                = TreeEntry (objectMode mode) (objectType ty) (OID oid) (unpack path)
                | otherwise = nullTreeEntry

sh :: MonadIO m => Sh a -> m a
sh = shelly . silently . onCommandHandles (initOutputHandles (`hSetBinaryMode` True))

newtype OID = OID Text
  deriving (Eq, Show, Ord)

data ObjectMode
  = NormalMode
  | ExecutableMode
  | SymlinkMode
  | TreeMode
  | OtherMode
  deriving (Eq, Show)

objectMode :: Text -> ObjectMode
objectMode "100644" = NormalMode
objectMode "100755" = ExecutableMode
objectMode "120000" = SymlinkMode
objectMode "040000" = TreeMode
objectMode _        = OtherMode

data ObjectType
  = BlobObject
  | TreeObject
  | OtherObjectType
  deriving (Eq, Show)

objectType :: Text -> ObjectType
objectType "blob" = BlobObject
objectType "tree" = TreeObject
objectType _      = OtherObjectType

data TreeEntry
  = TreeEntry
  { treeEntryMode :: ObjectMode
  , treeEntryType :: ObjectType
  , treeEntryOid  :: OID
  , treeEntryPath :: FilePath
  } deriving (Eq, Show)

nullTreeEntry :: TreeEntry
nullTreeEntry = TreeEntry OtherMode OtherObjectType (OID mempty) mempty
