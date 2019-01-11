{-# LANGUAGE DeriveAnyClass, DerivingVia #-}

module Data.GitHub.User
  ( User (..)
  , Type (..)
  , monalisa
  ) where

import Prologue

import Proto3.Suite
import Proto3.Suite.Exts

import Proto3.Google.Timestamp
import Data.GitHub.Spamurai (SpamuraiClassification (Hammy))

data Type
  = Unknown
  | Standard
  | Organization
  | Bot
    deriving (Eq, Show, Enum, Bounded, MessageField, Named, Generic)
    deriving Primitive via PrimitiveEnum Type

instance HasDefault Type where def = Unknown

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
  , userCreatedAt = Absent
  , userSuspended = False
  , userSpamuraiCalculation = Hammy
  , userAnalyticsTrackingId = "MACHHOMMY"
  }
