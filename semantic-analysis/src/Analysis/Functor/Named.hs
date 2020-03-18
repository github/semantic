{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Analysis.Functor.Named
( module Analysis.Name
, Named (..)
, named
, named'
, namedName
, namedValue
, name_
, value_
) where


import Analysis.Name
import Data.Function (on)
import Data.Generics.Product
import GHC.Generics (Generic)

-- | Annotates an @a@ with a 'Name'-provided name, which is ignored for '==' and 'compare'.
data Named a = Named Name a
  deriving (Foldable, Functor, Show, Traversable, Generic)

named :: Name -> a -> Named a
named = Named

named' :: Name -> Named Name
named' u = Named u u

namedName :: Named a -> Name
namedName (Named n _) = n

namedValue :: Named a -> a
namedValue (Named _ a) = a

value_ :: Lens' (Named a) a
value_ = position @2

name_ :: Lens' (Named a) Name
name_ = position @1

instance Eq a => Eq (Named a) where
  (==) = (==) `on` namedValue

instance Ord a => Ord (Named a) where
  compare = compare `on` namedValue

type Lens' s a = forall f . Functor f => (a -> f a) -> (s -> f s)
