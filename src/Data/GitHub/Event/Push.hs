{-# LANGUAGE DeriveAnyClass #-}

module Data.GitHub.Event.Push
  ( RepositoryPush (..)
  ) where

import Prologue

import Proto3.Suite

import qualified Data.GitHub.Git as Git
import Data.GitHub.Request.Context
import Data.GitHub.User

data ChangedFile = ChangedFile
  { filePreviousOID  :: Git.OID
  , fileOID          :: Git.OID
  , fileChangeType   :: Text -- TODO refine this, as it's always "A", "M", or "D"
  , filePath         :: Text
  , filePreviousPath :: Text
  } deriving (Eq, Show, Generic, Message, Named)

data RepositoryPush = RepositoryPush
  { pushRequestContext :: Nested RequestContext
  , pushActor          :: Nested User
  , pushBefore         :: Git.SHA
  , pushAfter          :: Git.SHA
  , pushRef            :: Text
  , pushChangedFiles   :: NestedVec ChangedFile
  } deriving (Eq, Show, Generic, Message, Named)
