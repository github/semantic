{-# LANGUAGE DataKinds, DefaultSignatures, GADTs, RankNTypes, TypeOperators #-}
module Algorithm where

import Control.Applicative (liftA2)
import Control.Monad (guard, join)
import Control.Monad.Free.Freer
import Data.Function (on)
import Data.Functor.Both
import Data.Functor.Classes
import Data.Maybe
import Data.Proxy
import Data.These
import Data.Union
import Diff
import GHC.Generics
import Term

-- | A single step in a diffing algorithm, parameterized by the types of terms, diffs, and the result of the applicable algorithm.
data AlgorithmF term diff result where
  -- | Diff two terms with the choice of algorithm left to the interpreter’s discretion.
  Diff :: term -> term -> AlgorithmF term diff diff
  -- | Diff two terms recursively in O(n) time, resulting in a single diff node.
  Linear :: term -> term -> AlgorithmF term diff diff
  -- | Diff two lists of terms by each element’s similarity in O(n³ log n), resulting in a list of diffs.
  RWS :: [term] -> [term] -> AlgorithmF term diff [diff]
  -- | Delete a term..
  Delete :: term -> AlgorithmF term diff diff
  -- | Insert a term.
  Insert :: term -> AlgorithmF term diff diff
  -- | Replace one term with another.
  Replace :: term -> term -> AlgorithmF term diff diff

-- | The free applicative for 'AlgorithmF'. This enables us to construct diff values using <$> and <*> notation.
type Algorithm term diff = Freer (AlgorithmF term diff)


-- DSL

-- | Diff two terms without specifying the algorithm to be used.
diff :: term -> term -> Algorithm term diff diff
diff = (liftF .) . Diff

-- | Diff a These of terms without specifying the algorithm to be used.
diffThese :: These term term -> Algorithm term diff diff
diffThese = these byDeleting byInserting diff

-- | Diff a pair of optional terms without specifying the algorithm to be used.
diffMaybe :: Maybe term -> Maybe term -> Algorithm term diff (Maybe diff)
diffMaybe a b = case (a, b) of
  (Just a, Just b) -> Just <$> diff a b
  (Just a, _) -> Just <$> byDeleting a
  (_, Just b) -> Just <$> byInserting b
  _ -> pure Nothing

-- | Diff two terms linearly.
linearly :: term -> term -> Algorithm term diff diff
linearly a b = liftF (Linear a b)

-- | Diff two terms using RWS.
byRWS :: [term] -> [term] -> Algorithm term diff [diff]
byRWS a b = liftF (RWS a b)

-- | Delete a term.
byDeleting :: term -> Algorithm term diff diff
byDeleting = liftF . Delete

-- | Insert a term.
byInserting :: term -> Algorithm term diff diff
byInserting = liftF . Insert

-- | Replace one term with another.
byReplacing :: term -> term -> Algorithm term diff diff
byReplacing = (liftF .) . Replace


instance Show term => Show1 (AlgorithmF term diff) where
  liftShowsPrec _ _ d algorithm = case algorithm of
    Diff t1 t2 -> showsBinaryWith showsPrec showsPrec "Diff" d t1 t2
    Linear t1 t2 -> showsBinaryWith showsPrec showsPrec "Linear" d t1 t2
    RWS as bs -> showsBinaryWith showsPrec showsPrec "RWS" d as bs
    Delete t1 -> showsUnaryWith showsPrec "Delete" d t1
    Insert t2 -> showsUnaryWith showsPrec "Insert" d t2
    Replace t1 t2 -> showsBinaryWith showsPrec showsPrec "Replace" d t1 t2


-- | Diff two terms based on their generic Diffable instances. If the terms are not diffable
-- (represented by a Nothing diff returned from algorithmFor) replace one term with another.
algorithmForTerms :: (Functor f, Diffable f) => Term f a -> Term f a -> Algorithm (Term f a) (Diff f a) (Diff f a)
algorithmForTerms t1 t2 = fromMaybe (byReplacing t1 t2) (fmap (wrap . (both ann1 ann2 :<)) <$> algorithmFor f1 f2)
  where ann1 :< f1 = runCofree t1
        ann2 :< f2 = runCofree t2


-- | A type class for determining what algorithm to use for diffing two terms.
class Diffable f where
  algorithmFor :: f term -> f term -> Maybe (Algorithm term diff (f diff))
  default algorithmFor :: (Generic1 f, Diffable' (Rep1 f)) => f term -> f term -> Maybe (Algorithm term diff (f diff))
  algorithmFor a b = fmap to1 <$> algorithmFor' (from1 a) (from1 b)

-- | Diff a Union of Syntax terms. Left is the "rest" of the Syntax terms in the Union,
-- Right is the "head" of the Union. 'weaken' relaxes the Union to allow the possible
-- diff terms from the "rest" of the Union, and 'inj' adds the diff terms into the Union.
-- NB: If Left or Right Syntax terms in our Union don't match, we fail fast by returning Nothing.
instance Apply1 Diffable fs => Diffable (Union fs) where
  algorithmFor u1 u2 = join (apply1_2' (Proxy :: Proxy Diffable) (\ reinj f1 f2 -> fmap reinj <$> algorithmFor f1 f2) u1 u2)

-- | Diff two list parameters using RWS.
instance Diffable [] where
  algorithmFor a b = Just (byRWS a b)

-- | A generic type class for diffing two terms defined by the Generic1 interface.
class Diffable' f where
  algorithmFor' :: f term -> f term -> Maybe (Algorithm term diff (f diff))

-- | Diff two constructors (M1 is the Generic1 newtype for meta-information (possibly related to type constructors, record selectors, and data types))
instance Diffable' f => Diffable' (M1 i c f) where
  algorithmFor' (M1 a) (M1 b) = fmap M1 <$> algorithmFor' a b

-- | Diff the fields of a product type.
-- i.e. data Foo a b = Foo a b (the 'Foo a b' is captured by 'a :*: b').
instance (Diffable' f, Diffable' g) => Diffable' (f :*: g) where
  algorithmFor' (a1 :*: b1) (a2 :*: b2) = liftA2 (:*:) <$> algorithmFor' a1 a2 <*> algorithmFor' b1 b2

-- | Diff the constructors of a sum type.
-- i.e. data Foo a = Foo a | Bar a (the 'Foo a' is captured by L1 and 'Bar a' is R1).
instance (Diffable' f, Diffable' g) => Diffable' (f :+: g) where
  algorithmFor' (L1 a) (L1 b) = fmap L1 <$> algorithmFor' a b
  algorithmFor' (R1 a) (R1 b) = fmap R1 <$> algorithmFor' a b
  algorithmFor' _ _ = Nothing

-- | Diff two parameters (Par1 is the Generic1 newtype representing a type parameter).
-- i.e. data Foo a = Foo a (the 'a' is captured by Par1).
instance Diffable' Par1 where
  algorithmFor' (Par1 a) (Par1 b) = Just (Par1 <$> linearly a b)

-- | Diff two constant parameters (K1 is the Generic1 newtype representing type parameter constants).
-- i.e. data Foo = Foo Int (the 'Int' is a constant parameter).
instance Eq c => Diffable' (K1 i c) where
  algorithmFor' (K1 a) (K1 b) = guard (a == b) *> Just (pure (K1 a))

-- | Diff two terms whose constructors contain 0 type parameters.
-- i.e. data Foo = Foo.
instance Diffable' U1 where
  algorithmFor' _ _ = Just (pure U1)

-- | Diff two recursively defined parameters (Rec1 is the Generic1 newtype representing recursive type parameters).
-- i.e. data Tree a = Leaf a | Node (Tree a) (Tree a) (the two 'Tree a' in 'Node (Tree a) (Tree a)' are Rec1 type parameters).
instance Diffable' (Rec1 []) where
  algorithmFor' a b = fmap Rec1 <$> Just ((byRWS `on` unRec1) a b)
