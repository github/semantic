{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.GitHub.User
  ( User (..)
  , SpamuraiClassification (..)
  , monalisa
  ) where

import Prologue

import Proto3.Suite

import Data.GitHub.Timestamp
import Data.GitHub.User.Type (Type (Bot))
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

monalisa :: User
monalisa = User
  { userId = 11111
  , userLogin = "monalisa"
  , userType = Bot
  , userBillingPlan = "medium"
  , userSpammy = False
  , userTimestamp = Nested Nothing
  , userSuspended = False
  , userSpamuraiCalculation = Hammy
  , userAnalyticsTrackingId = "MACHHOMMY"
  }
