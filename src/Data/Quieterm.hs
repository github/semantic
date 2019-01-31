{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
module Data.Quieterm
( Quieterm(..)
, quieterm
) where

import Control.DeepSeq
import Data.Abstract.Declarations (Declarations)
import Data.Abstract.FreeVariables (FreeVariables)
import Data.Abstract.AccessControls.Class
import Data.Abstract.AccessControls.Instances ()
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Term
import Text.Show (showListWith)

newtype Quieterm syntax ann = Quieterm { unQuieterm :: TermF syntax ann (Quieterm syntax ann) }
  deriving (Declarations, FreeVariables, AccessControls)

type instance Base (Quieterm syntax ann) = TermF syntax ann
instance Functor syntax => Recursive   (Quieterm syntax ann) where project = unQuieterm
instance Functor syntax => Corecursive (Quieterm syntax ann) where embed   =   Quieterm

instance Eq1 syntax => Eq1 (Quieterm syntax) where
  liftEq eqA = go where go t1 t2 = liftEq2 eqA go (unQuieterm t1) (unQuieterm t2)

instance (Eq1 syntax, Eq ann) => Eq (Quieterm syntax ann) where
  (==) = eq1

instance Ord1 syntax => Ord1 (Quieterm syntax) where
  liftCompare comp = go where go t1 t2 = liftCompare2 comp go (unQuieterm t1) (unQuieterm t2)

instance (Ord1 syntax, Ord ann) => Ord (Quieterm syntax ann) where
  compare = compare1

instance Show1 syntax => Show1 (Quieterm syntax) where
  liftShowsPrec _ _ = go where go d = liftShowsPrec go (showListWith (go 0)) d . termFOut . unQuieterm

instance Show1 syntax => Show (Quieterm syntax ann) where
  showsPrec = liftShowsPrec (const (const id)) (const id)

instance NFData1 f => NFData1 (Quieterm f) where
  liftRnf rnf = go where go x = liftRnf2 rnf go (unQuieterm x)

instance (NFData1 f, NFData a) => NFData (Quieterm f a) where
  rnf = liftRnf rnf

quieterm :: (Recursive term, Base term ~ TermF syntax ann) => term -> Quieterm syntax ann
quieterm = cata Quieterm
