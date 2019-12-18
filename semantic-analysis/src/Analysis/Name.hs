{-# LANGUAGE DeriveTraversable, GeneralizedNewtypeDeriving #-}
module Analysis.Name
( Name(..)
, Named(..)
, named
, named'
, namedName
, namedValue
) where

import Data.Function (on)
import Data.String (IsString)
import Data.Text (Text)

-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, IsString, Ord, Show)


-- | Annotates an @a@ with a 'Name'-provided name, which is ignored for '==' and 'compare'.
data Named a = Named Name a
  deriving (Foldable, Functor, Show, Traversable)

named :: Name -> a -> Named a
named = Named

named' :: Name -> Named Name
named' u = Named u u

namedName :: Named a -> Name
namedName (Named n _) = n

namedValue :: Named a -> a
namedValue (Named _ a) = a

instance Eq a => Eq (Named a) where
  (==) = (==) `on` namedValue

instance Ord a => Ord (Named a) where
  compare = compare `on` namedValue
