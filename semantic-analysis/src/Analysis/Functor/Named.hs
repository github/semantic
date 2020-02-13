{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Analysis.Functor.Named
( module Analysis.Name
, Named (..)
, named
, named'
, namedName
, namedValue
) where


import Analysis.Name
import Data.Function (on)

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
