{-# LANGUAGE DeriveAnyClass, DerivingVia, OverloadedLists #-}

module Data.GitHub.Event.RepositoryPush
  ( ChangedFile (..)
  , RepositoryPush (..)
  ) where

import Prologue

import Proto3.Suite

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

data RepositoryPush = RepositoryPush
  { pushRequestContext :: Nested RequestContext
  , pushOwner          :: Nested User
  , pushActor          :: Nested User
  , pushRepository     :: Nested Repository
  , pushBefore         :: Git.SHA
  , pushAfter          :: Git.SHA
  , pushRef            :: Git.Ref
  , pushChangedFiles   :: NestedVec ChangedFile
  } deriving (Eq, Show, Generic, Message, Named)
