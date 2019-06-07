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
  run "git" ["-C", pack gitDir, "cat-file", "-p", oid]

-- | git ls-tree -rz
lsTree :: FilePath -> OID -> IO [TreeEntry]
lsTree gitDir (OID sha) = sh $ parseEntries <$> run "git" ["-C", pack gitDir, "ls-tree", "-rz", sha]

sh :: MonadIO m => Sh a -> m a
sh = shelly . silently . onCommandHandles (initOutputHandles (`hSetBinaryMode` True))

-- | Parses an list of entries separated by \NUL, and on failure return []
parseEntries :: Text -> [TreeEntry]
parseEntries text = case parseOnly everything text of
  Done "" ls -> ls
  other      -> error ("There was an error parsing the Git output: " <> show other)
  where
    everything = AP.sepBy entryParser "\NUL" <* ("\NUL\n" <?> "End sequence") <* AP.endOfInput <?> "Everything"
    parseOnly p t = AP.feed (AP.parse p t) ""

-- | Parse the entire input with entryParser, and on failure return a default
-- For testing purposes only
parseEntry :: Text -> TreeEntry
parseEntry = either (const nullTreeEntry) id . AP.parseOnly (entryParser <* AP.endOfInput)

-- | Parses a TreeEntry
entryParser :: Parser TreeEntry
entryParser = TreeEntry
  <$> modeParser <* (" " <?> "First Space")
  <*> typeParser <* (" " <?> "Second Space")
  <*> (OID <$> AP.takeWhile (AP.inClass "0123456789abcdef") <?> "OID Parser") <* ("\t" <?> "Tab")
  <*> (unpack <$> AP.takeWhile (/= '\NUL') <?> "Filepath") <?> "Entry Parser"
    where
      typeParser = AP.choice [BlobObject <$ "blob", TreeObject <$ "tree"] <?> "Type Parser"
      modeParser = AP.choice [NormalMode <$ "100644", ExecutableMode <$ "100755", SymlinkMode <$ "120000", TreeMode <$ "040000"] <?> "Mode Parser"

newtype OID = OID Text
  deriving (Eq, Show, Ord)

data ObjectMode
  = NormalMode
  | ExecutableMode
  | SymlinkMode
  | TreeMode
  | OtherMode
  deriving (Eq, Show)

data ObjectType
  = BlobObject
  | TreeObject
  | OtherObjectType
  deriving (Eq, Show)

data TreeEntry
  = TreeEntry
  { treeEntryMode :: ObjectMode
  , treeEntryType :: ObjectType
  , treeEntryOid  :: OID
  , treeEntryPath :: FilePath
  } deriving (Eq, Show)

nullTreeEntry :: TreeEntry
nullTreeEntry = TreeEntry OtherMode OtherObjectType (OID mempty) mempty
