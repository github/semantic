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

import           Data.Attoparsec.ByteString (Parser)
import           Data.Attoparsec.ByteString as AP
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (w2c)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as B
import qualified Data.ByteString.Streaming as ByteStream
import qualified Data.Attoparsec.ByteString.Streaming as AP.Streaming
import           Data.Char
import           Data.Either (fromRight)
import           Data.Text as Text
import           Text.Parser.Combinators (sepEndBy)
import qualified Streaming.Process
import qualified System.Process as Process
import qualified Source.Source as Source

-- | git clone --bare
clone :: Text -> FilePath -> IO ()
clone url path = Process.callProcess "git"
  ["clone", "--bare", Text.unpack url, path]

-- | Runs @git cat-file -p@. Throws 'ProcessExitedUnsuccessfully' if the
-- underlying git command returns a nonzero exit code. Loads the contents
-- of the file into memory all at once and strictly.
catFile :: FilePath -> OID -> IO Source.Source
catFile gitDir (OID oid) =
  let process = Process.proc "git" ["-C", gitDir, "cat-file", "-p", UTF8.toString oid]
      consumeStdout stream = Streaming.Process.withProcessOutput stream ByteStream.toStrict_
  in Source.fromUTF8 <$> Streaming.Process.withStreamProcess process consumeStdout

-- | git ls-tree -rz
lsTree :: FilePath -> OID -> IO [TreeEntry]
lsTree gitDir (OID sha) =
  let process        = Process.proc "git" ["-C", gitDir, "ls-tree", "-rz", UTF8.toString sha]
      allEntries     = (entryParser `sepEndBy` AP.word8 0) <* AP.endOfInput
      ignoreFailures = fmap (fromRight [] . fst)
  in Streaming.Process.withStreamProcess process $
     \stream -> Streaming.Process.withProcessOutput stream (ignoreFailures . AP.Streaming.parse allEntries)


-- | Parses an list of entries separated by \NUL, and on failure return []
parseEntries :: ByteString -> [TreeEntry]
parseEntries = fromRight [] . AP.parseOnly everything
  where
    everything = AP.sepBy entryParser (AP.word8 0)

-- | Parse the entire input with entryParser, and on failure return a default
-- For testing purposes only
parseEntry :: ByteString -> Either String TreeEntry
parseEntry = AP.parseOnly (entryParser <* AP.endOfInput)

-- | Parses a TreeEntry
entryParser :: Parser TreeEntry
entryParser = TreeEntry
  <$> modeParser <* AP.word8 space
  <*> typeParser <* AP.word8 space
  <*> oidParser <* AP.word8 tab
  <*> (UTF8.toString <$> AP.takeWhile (/= nul))
    where
      char = fromIntegral @Int @Word8 . ord
      space = char ' '
      tab = char '\t'
      nul = char '\NUL'
      typeParser = AP.choice [BlobObject <$ "blob", TreeObject <$ "tree", OtherObjectType <$ AP.takeWhile (isAlphaNum . w2c)]
      modeParser = AP.choice [NormalMode <$ "100644", ExecutableMode <$ "100755", SymlinkMode <$ "120000", TreeMode <$ "040000", OtherMode <$ AP.takeWhile (isAlphaNum . w2c)]
      oidParser = OID <$> AP.takeWhile (isHexDigit . w2c)

newtype OID = OID ByteString
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
