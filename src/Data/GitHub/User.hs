{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.GitHub.User
  ( User (..)
  , SpamuraiClassification (..)
  , monalisa
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

import Data.GitHub.Timestamp
import Data.GitHub.User.Type (Type (Bot))
import Data.GitHub.Spamurai

data User = User
  { userId                  :: Word32
  , userLogin               :: Text
  , userType                :: Type
  , userBillingPlan         :: Text
  , userSpammy              :: Bool
  , userGlobalRelayId       :: Text
  , userCreatedAt           :: Nested Timestamp
  , userSuspended           :: Bool
  , userSpamuraiCalculation :: SpamuraiClassification
  , userAnalyticsTrackingId :: Text
  } deriving (Eq, Show, Generic, Message, Named)

monalisa :: User
monalisa = User
  { userId = 1
  , userLogin = "monalisa"
  , userType = Bot
  , userBillingPlan = "medium"
  , userSpammy = False
  , userGlobalRelayId = "global"
  , userCreatedAt = lowerBound
  , userSuspended = False
  , userSpamuraiCalculation = Hammy
  , userAnalyticsTrackingId = "MACHHOMMY"
  }
