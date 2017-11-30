{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, MonoLocalBinds #-}
module Abstract.Interpreter where

import Abstract.Environment
import Abstract.Eval
import Abstract.FreeVariables
import Abstract.Monad.Env
import Abstract.Store
import Abstract.Type
import Abstract.Value

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Semigroup
import qualified Data.Set as Set
import Data.Term
import Prelude hiding (fail)


type Interpreter l v = '[Fresh, Fail, NonDetEff, State (Store l v), Reader (Set.Set (Address l v)), Reader (Environment l v)]

type MonadInterpreter l v m = (MonadEnv l v m, MonadStore v m, MonadFail m)

type EvalResult l v = Final (Interpreter l v) v

type Eval' t m v = (v -> m v) -> t -> m v

-- Evaluate an expression.
-- Example:
--    evaluate @Type <term>
--    evaluate @(Value (Data.Union.Union Language.Python.Assignment2.Syntax) (Record Location) Precise) <term>
evaluate :: forall v syntax ann
         . ( Ord v
           , Functor syntax
           , Semigroup (Cell (LocationFor v) v)
           , FreeVariables1 syntax
           , MonadAddress (LocationFor v) (Eff (Interpreter (LocationFor v) v))
           , Eval (Term syntax ann) v (Eff (Interpreter (LocationFor v) v)) syntax
           )
         => Term syntax ann
         -> EvalResult (LocationFor v) v
evaluate = run @(Interpreter (LocationFor v) v) . fix ev pure

ev ::
     ( Functor syntax
     , FreeVariables1 syntax
     , Eval (Term syntax ann) v m syntax
     )
     => Eval' (Term syntax ann) m v -> Eval' (Term syntax ann) m v
ev recur yield = eval recur yield . unTerm

evCollect :: forall t v m
          .  ( Ord (LocationFor v)
             , Foldable (Cell (LocationFor v))
             , MonadStore v m
             , MonadGC (LocationFor v) v m
             , ValueRoots (LocationFor v) v
             )
          => (Eval' t m v -> Eval' t m v)
          -> Eval' t m v
          -> Eval' t m v
evCollect ev0 ev' yield e = do
  roots <- askRoots :: m (Set.Set (Address (LocationFor v) v))
  v <- ev0 ev' yield e
  modifyStore (gc (roots <> valueRoots v))
  return v

evRoots :: forall l v m syntax ann
        .  ( Ord l
           , MonadEnv l v m
           , MonadGC l v m
           , ValueRoots l v
           , Eval (Term syntax ann) v m (TermF syntax ann)
           , FreeVariables1 syntax
           , Functor syntax
           )
        => Eval' (Term syntax ann) m v
        -> Eval' (Term syntax ann) m v
evRoots ev' yield = eval ev' yield . unTerm

gc :: (Ord l, Foldable (Cell l), ValueRoots l a) => Set.Set (Address l a) -> Store l a -> Store l a
gc roots store = storeRestrict store (reachable roots store)

reachable :: (Ord l, Foldable (Cell l), ValueRoots l a) => Set.Set (Address l a) -> Store l a -> Set.Set (Address l a)
reachable roots store = go roots mempty
  where go set seen = case Set.minView set of
          Nothing -> seen
          Just (a, as)
            | Just values <- storeLookupAll a store -> go (Set.difference (foldr ((<>) . valueRoots) mempty values <> as) seen) (Set.insert a seen)
            | otherwise -> go seen (Set.insert a seen)
