{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Collecting where

import Abstract.Environment
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Data.Term
import Abstract.Value
import Abstract.Eval

import Control.Monad.Effect
import Control.Monad.Effect.Reader
import Data.Semigroup


instance (Ord l, Reader (Set (Address l a)) :< fs) => MonadGC l a (Eff fs) where
  askRoots = ask :: Eff fs (Set (Address l a))

  extraRoots roots' = local (<> roots')

gc :: (Ord l, Foldable (Cell l), AbstractValue l a) => Set (Address l a) -> Store l a -> Store l a
gc roots store = storeRestrict store (reachable roots store)

reachable :: (Ord l, Foldable (Cell l), AbstractValue l a) => Set (Address l a) -> Store l a -> Set (Address l a)
reachable roots store = go roots mempty
  where go set seen = case split set of
          Nothing -> seen
          Just (a, as)
            | Just values <- storeLookupAll a store -> go (difference (foldr ((<>) . valueRoots) mempty values <> as) seen) (insert a seen)
            | otherwise -> go seen (insert a seen)


evCollect :: forall l t v m
          .  ( Ord l
             , Foldable (Cell l)
             , MonadStore l v m
             , MonadGC l v m
             , AbstractValue l v
             )
          => (Eval' t (m v) -> Eval' t (m v))
          -> Eval' t (m v)
          -> Eval' t (m v)
evCollect ev0 ev e = do
  roots <- askRoots :: m (Set (Address l v))
  v <- ev0 ev e
  modifyStore (gc (roots <> valueRoots v))
  return v

evRoots :: forall l v m syntax ann
        .  ( Ord l
           , MonadEnv l v m
           , MonadGC l v m
           , MonadPrim v m
           , AbstractValue l v
           , EvalCollect l v m syntax ann (TermF syntax ann)
           )
        => Eval' (Term syntax ann) (m v)
        -> Eval' (Term syntax ann) (m v)
evRoots ev = evalCollect @l ev . unTerm
