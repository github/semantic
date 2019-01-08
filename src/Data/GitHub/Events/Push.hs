{-# LANGUAGE DeriveAnyClass #-}

module Data.GitHub.Events.Push
  ( RepositoryPush (..)
  ) where

import Prologue

import Proto3.Suite

import Data.GitHub.Request.Context

newtype SHA = SHA { shaContents :: Text }

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
  , pushBefore         :: SHA
  , pushAfter          :: SHA
  , pushRef            :: Text
  , pushChangedFiles   :: NestedVec ChangedFile
  } deriving (Eq, Show, Generic, Message, Named)
