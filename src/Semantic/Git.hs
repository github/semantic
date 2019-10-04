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

import Prologue

import           Data.Attoparsec.Text (Parser)
import           Data.Attoparsec.Text as AP
import qualified Data.ByteString.Streaming as ByteStream
import           Data.Char
import           Data.Either (fromRight)
import           Data.Text as Text
import           Shelly hiding (FilePath)
import           Streaming hiding (run)
import qualified Streaming.Process
import           System.Exit
import qualified System.Process as Process (proc)
import qualified Source.Source as Source

-- | git clone --bare
clone :: Text -> FilePath -> IO ()
clone url path = sh $ do
  run_ "git" ["clone", "--bare", url, pack path]

-- | Runs @git cat-file -p@. Throws 'ProcessExitedUnsuccessfully' if the
-- underlying git command returns a nonzero exit code. Loads the contents
-- of the file into memory all at once and strictly.
catFile :: FilePath -> OID -> IO Source.Source
catFile gitDir (OID oid) =
  let process = Process.proc "git" ["-C", gitDir, "cat-file", "-p", Text.unpack oid]
      consumeStdout stream = Streaming.Process.withProcessOutput stream ByteStream.toStrict_
  in Source.fromUTF8 <$> Streaming.Process.withStreamProcess process consumeStdout

-- | git ls-tree -rz
lsTree :: FilePath -> OID -> IO [TreeEntry]
lsTree gitDir (OID sha) = sh $ parseEntries <$> run "git" ["-C", pack gitDir, "ls-tree", "-rz", sha]

sh :: MonadIO m => Sh a -> m a
sh = shelly . silently

-- | Parses an list of entries separated by \NUL, and on failure return []
parseEntries :: Text -> [TreeEntry]
parseEntries = fromRight [] . AP.parseOnly everything
  where
    everything = AP.sepBy entryParser "\NUL" <* "\NUL\n" <* AP.endOfInput

-- | Parse the entire input with entryParser, and on failure return a default
-- For testing purposes only
parseEntry :: Text -> Either String TreeEntry
parseEntry = AP.parseOnly (entryParser <* AP.endOfInput)

-- | Parses a TreeEntry
entryParser :: Parser TreeEntry
entryParser = TreeEntry
  <$> modeParser <* AP.char ' '
  <*> typeParser <* AP.char ' '
  <*> oidParser <* AP.char '\t'
  <*> (unpack <$> AP.takeWhile (/= '\NUL'))
    where
      typeParser = AP.choice [BlobObject <$ "blob", TreeObject <$ "tree", OtherObjectType <$ AP.takeWhile isAlphaNum]
      modeParser = AP.choice [NormalMode <$ "100644", ExecutableMode <$ "100755", SymlinkMode <$ "120000", TreeMode <$ "040000", OtherMode <$ AP.takeWhile isAlphaNum]
      oidParser = OID <$> AP.takeWhile isHexDigit

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
