{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Analysis.Name
( Name(..)
) where

import Data.String (IsString)
import Data.Text (Text)

-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, IsString, Ord, Show)


