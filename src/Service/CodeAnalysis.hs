{-# LANGUAGE DerivingStrategies, DeriveAnyClass #-}

module Service.CodeAnalysis (
    PingRequest(..)
  , PingResponse(..)
  , ParseTreeRequest(..)
  , ParseTreeSymbolResponse(..)
  , File(..)
  , Symbol(..)
  )
where

import           Data.Aeson
import           Data.Blob
import           GHC.Generics
import qualified Proto3.Suite as Proto3
import           Rendering.Symbol (File, Symbol)

newtype ParseTreeRequest = ParseTreeRequest { blobs :: [Blob] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Proto3.Message, Proto3.Named, FromJSON)

newtype ParseTreeSymbolResponse
  = ParseTreeSymbolResponse
  { files :: [File] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Proto3.Message, Proto3.Named, ToJSON)

newtype PingRequest = PingRequest { service :: String }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Proto3.Message, Proto3.Named, FromJSON)

data PingResponse
  = PingResponse
  { status :: String
  , hostname :: String
  , timestamp :: String
  , sha :: String
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Proto3.Message, Proto3.Named, ToJSON)
