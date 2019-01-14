{-# LANGUAGE DeriveAnyClass, DerivingVia, OverloadedLists #-}

module Data.GitHub.Event.RepositoryPush where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

import qualified Data.GitHub.Git as Git
import Data.GitHub.Request.Context
import Data.GitHub.User
import Data.GitHub.Repository

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
  , pushRef            :: Git.Ref
  , pushChangedFiles   :: NestedVec ChangedFile
  } deriving (Eq, Show, Generic, Message, Named)

samplePush :: RepositoryPush
samplePush = RepositoryPush
  { pushRequestContext = Absent
  , pushActor = pure monalisa
  , pushRepository = pure sampleRepository
  , pushBefore = Git.SHA "308cf87a25e096a71be8aba8bb53f5575a60c388"
  , pushAfter = Git.SHA "d7d56aeb77d6b8434d2789b4b0b6b305dfce10a82"
  , pushRef = Git.Ref "refs/heads/test-branch"
  , pushChangedFiles = [sampleChange]
  }
