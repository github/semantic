{-# LANGUAGE DeriveGeneric #-}
module Stack.File
  ( File (..)

  ) where

import           Data.Text (Text)
import           Data.Vector (Vector)
import           GHC.Generics (Generic)
import           Stack.Node (Node)
import           Stack.Path (Path)
import qualified System.Path as Path

type ParseError = Text

data File = File
  { path     :: Path.AbsRelFile
  , language :: Text
  , nodes    :: Vector Node
  , paths    :: Vector Path
  , errors   :: Vector ParseError
  } deriving (Eq, Show, Generic)
