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

  -- Testing Purposes
  , parseEntries
  , parseEntry
  ) where

import Control.Monad.IO.Class
import Data.Attoparsec.Text   (Parser)
import Data.Attoparsec.Text   as AP
import Data.Text              as Text
import Shelly                 hiding (FilePath)
import System.IO              (hSetBinaryMode)

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
  pure $ parseEntries out

sh :: MonadIO m => Sh a -> m a
sh = shelly . silently . onCommandHandles (initOutputHandles (`hSetBinaryMode` True))

-- | Parses an list of entries separated by \NUL, and on failure return []
parseEntries :: Text -> [TreeEntry]
parseEntries = either (const []) id . AP.parseOnly (AP.sepBy entryParser (AP.char '\NUL'))

-- | Parse the entire input with entryParser, and on failure return a default
-- For testing purposes only
parseEntry :: Text -> TreeEntry
parseEntry = either (const nullTreeEntry) id . AP.parseOnly entryParser

-- | Parses an entry successfully, falling back to the failure case if necessary.
entryParser :: Parser TreeEntry
entryParser = AP.choice [entrySuccessParser, entryDefaultParser]

-- | Attoparsec parser for a block af text ending with \NUL
-- in order to consume invalid input
entryDefaultParser :: Parser TreeEntry
entryDefaultParser = do
  _ <- AP.takeWhile (/= '\NUL')
  pure $ nullTreeEntry

-- | Attoparsec parser for a single line of git ls-tree -rz output
entrySuccessParser :: Parser TreeEntry
entrySuccessParser = do
  mode <- takeWhileToNul (/= ' ')
  _ <- AP.char ' '
  ty <- takeWhileToNul (/= ' ')
  _ <- AP.char ' '
  oid <- takeWhileToNul (/= '\t')
  _ <- AP.char '\t'
  path <- takeWhileToNul (const True)
  pure $ TreeEntry (objectMode mode) (objectType ty) (OID oid) (unpack path)
    where
      takeWhileToNul f = AP.takeWhile (\x -> f x && x /= '\NUL')

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
