{-# LANGUAGE DeriveAnyClass #-}

module Data.GitHub.Request.Context
  ( RequestContext (..)
  ) where

import Prologue

import Proto3.Suite

import Proto3.Google.Wrapped
import Data.GitHub.Request.Method
import Data.GitHub.Auth
import Data.GitHub.IPVersion

data RequestContext = RequestContext
  { requestID               :: Text
  , requestMethod           :: RequestMethod
  , requestURL              :: Text
  , requestIPAddress        :: Text
  , requestIPVersion        :: IPVersion
  , requestV4Int            :: Word32
  , requestV6Int            :: ByteString
  , requestUserAgent        :: Text
  , requestSessionID        :: Word32
  , requestController       :: Text
  , requestControllerAction :: Text
  , requestAPIRoute         :: Text
  , requestCategory         :: Text
  , requestFrom             :: Text
  , requestAuth             :: AuthTypes
  , requestClientID         :: Text
  , requestReferrer         :: Text
  , requestCountryCode      :: Nested (Wrapped Text)
  , requestCountryName      :: Nested (Wrapped Text)
  , requestRegion           :: Nested (Wrapped Text)
  , requestRegionName       :: Nested (Wrapped Text)
  , requestCity             :: Nested (Wrapped Text)
  } deriving (Eq, Show, Generic, Named, Message)
