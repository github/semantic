{-# LANGUAGE DeriveAnyClass, DerivingVia, OverloadedLists #-}

module Data.GitHub.Event.Push where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

import qualified Data.GitHub.Git as Git
import Data.GitHub.Timestamp
import Data.GitHub.Request.Context
import Data.GitHub.User
import Data.GitHub.Repository
import Proto3.Google.Wrapped

data Site
  = Unknown
  | Localhost
  | CP1_IAD
  | SDC42_SEA
    deriving (Eq, Show, Ord, Enum, Bounded, Generic, MessageField, Named)
    deriving Primitive via PrimitiveEnum Site

instance HasDefault Site where def = minBound

data Envelope = Envelope
  { envelopeTypeURL   :: Text
  , envelopeMessage   :: ByteString
  , envelopeId        :: Text
  , envelopeTimestamp :: Nested Timestamp
  , envelopeSite      :: Site
  } deriving (Eq, Show, Generic, Named, Message)


data ChangedFile = ChangedFile
  { filePreviousOID  :: Git.OID
  , fileOID          :: Git.OID
  , fileChangeType   :: Text -- TODO refine this, as it's always "A", "M", or "D"
  , filePath         :: Text
  , filePreviousPath :: Text
  } deriving (Eq, Show, Generic, Message, Named)

sampleChange :: ChangedFile
sampleChange = ChangedFile
  { filePreviousOID = Git.OID "0000000000000000000000000000000000000000"
  , fileOID = Git.OID "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"
  , fileChangeType = "A"
  , filePath = "dummyfile"
  , filePreviousPath = "/dev/null"
  }

data RepositoryPush = RepositoryPush
  { pushRequestContext :: Nested RequestContext
  , pushActor          :: Nested User
  , pushRepository     :: Nested Repository
  , pushBefore         :: Git.SHA
  , pushAfter          :: Git.SHA
  , pushRef            :: Text
  , pushChangedFiles   :: NestedVec ChangedFile
  } deriving (Eq, Show, Generic, Message, Named)

samplePush :: RepositoryPush
samplePush = RepositoryPush
  { pushRequestContext = lowerBound
  , pushActor = pure monalisa
  , pushRepository = pure sampleRepository
  , pushBefore = "308cf87a25e096a71be8aba8bb53f5575a60c388"
  , pushAfter = "d7d56aeb77d6b8434d2789b4b0b6b305dfce10a82"
  , pushRef = "refs/heads/test-branch"
  , pushChangedFiles = [sampleChange]
  }
