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

import           Control.Monad.IO.Class
import           Data.Char (isSpace)
import qualified Data.Attoparsec.Text as Parser
import           Data.Text as Text
import           Shelly hiding (FilePath)
import           System.IO (hSetBinaryMode)

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
  pure $ either mempty id (Parser.parseOnly treeEntriesParser out)


treeEntryParser :: Parser.Parser TreeEntry
treeEntryParser = do
  mode <- Parser.takeTill isSpace
  Parser.skipSpace
  ty <- Parser.takeTill isSpace
  Parser.skipSpace
  oid <- Parser.takeTill (== '\t')
  _ <- Parser.char '\t'
  Parser.skipSpace
  path <- Parser.takeTill (== '\NUL')
  pure $ TreeEntry (objectMode mode) (objectType ty) (OID oid) (Text.unpack path)


treeEntriesParser :: Parser.Parser [TreeEntry]
treeEntriesParser =  Parser.sepBy treeEntryParser (Parser.char '\NUL')


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
