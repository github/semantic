module Data.Functor.Classes.Show.Generic
( Show1(..)
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
