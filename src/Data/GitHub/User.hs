{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.GitHub.User
  ( User (..)
  , SpamuraiClassification (..)
  ) where

import Prologue

import Proto3.Suite

import Data.GitHub.Timestamp
import Data.GitHub.User.Type (Type)
import Data.GitHub.Spamurai

data User = User
  { userId                  :: Word32
  , userLogin               :: Text
  , userType                :: Type
  , userBillingPlan         :: Text
  , userSpammy              :: Bool
  , userTimestamp           :: Nested Timestamp
  , userSuspended           :: Bool
  , userSpamuraiCalculation :: SpamuraiClassification
  , userAnalyticsTrackingId :: Text
  } deriving (Eq, Show, Generic, Message, Named)
