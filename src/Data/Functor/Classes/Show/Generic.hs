{-# LANGUAGE TypeOperators #-}
module Data.Functor.Classes.Show.Generic
( Show1(..)
, genericLiftShowsPrec
, genericLiftShowList
, gliftShowsPrec
, gliftShowList
) where

import Data.Functor.Classes
import GHC.Generics
import Prologue
import Text.Show

-- | Generically-derivable lifting of the 'Show' class to unary type constructors.
class GShow1 f where
  -- | showsPrec function for an application of the type constructor based on showsPrec and showList functions for the argument type.
  gliftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS

  -- | showList function for an application of the type constructor based on showsPrec and showList functions for the argument type. The default implementation using standard list syntax is correct for most types.
  gliftShowList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
  gliftShowList sp sl = showListWith (gliftShowsPrec sp sl 0)

-- | A suitable implementation of Show1’s liftShowsPrec for Generic1 types.
genericLiftShowsPrec :: (Generic1 f, GShow1 (Rep1 f)) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
genericLiftShowsPrec sp sl d = gliftShowsPrec sp sl d . from1

-- | A suitable implementation of Show1’s liftShowsPrec for Generic1 types.
genericLiftShowList :: (Generic1 f, GShow1 (Rep1 f)) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
genericLiftShowList sp sl = gliftShowList sp sl . map from1


-- Show1 instances

instance GShow1 [] where gliftShowsPrec = liftShowsPrec
instance GShow1 Maybe where gliftShowsPrec = liftShowsPrec
instance Show a => GShow1 ((,) a) where gliftShowsPrec = liftShowsPrec
instance Show a => GShow1 (Either a) where gliftShowsPrec = liftShowsPrec


-- Generics

instance GShow1 U1 where
  gliftShowsPrec _ _ _ _ = identity

instance GShow1 Par1 where
  gliftShowsPrec sp _ d (Par1 a) = sp d a

instance Show c => GShow1 (K1 i c) where
  gliftShowsPrec _ _ d (K1 a) = showsPrec d a

instance Show1 f => GShow1 (Rec1 f) where
  gliftShowsPrec sp sl d (Rec1 a) = liftShowsPrec sp sl d a

instance GShow1 f => GShow1 (M1 D c f) where
  gliftShowsPrec sp sl d (M1 a) = gliftShowsPrec sp sl d a

instance (Constructor c, GShow1 f) => GShow1 (M1 C c f) where
  gliftShowsPrec sp sl d m = gliftShowsPrec sp sl d (unM1 m)

instance GShow1 f => GShow1 (M1 S c f) where
  gliftShowsPrec sp sl d (M1 a) = gliftShowsPrec sp sl d a

instance (GShow1 f, GShow1 g) => GShow1 (f :+: g) where
  gliftShowsPrec sp sl d (L1 l) = gliftShowsPrec sp sl d l
  gliftShowsPrec sp sl d (R1 r) = gliftShowsPrec sp sl d r

instance (GShow1 f, GShow1 g) => GShow1 (f :*: g) where
  gliftShowsPrec sp sl d (a :*: b) = gliftShowsPrec sp sl d a . showChar ' ' . gliftShowsPrec sp sl d b

instance (Show1 f, GShow1 g) => GShow1 (f :.: g) where
  gliftShowsPrec sp sl d (Comp1 a) = liftShowsPrec (gliftShowsPrec sp sl) (gliftShowList sp sl) d a
