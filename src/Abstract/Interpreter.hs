{-# LANGUAGE UndecidableInstances, AllowAmbiguousTypes, ConstraintKinds, DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeOperators, MonoLocalBinds #-}
module Abstract.Interpreter where

import Abstract.Environment
import Abstract.Eval
import Abstract.FreeVariables
import Abstract.Primitive
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


type Interpreter l v = '[Fresh, Fail, NonDetEff, State (Store l v), Reader (Environment l v)]

type MonadInterpreter l v m = (MonadEnv l v m, MonadStore l v m, MonadFail m)

type EvalResult l v = Final (Interpreter l v) v

type Eval' t m v = (v -> m v) -> t -> m v

-- Evaluate an expression.
-- Example:
--    Files.readFile "test.py" (Just Python) >>= runTask . parse pythonParser2 >>= pure . evaluate @Precise @(Value (Data.Union.Union Language.Python.Assignment2.Syntax) (Record Location) Precise)
evaluate :: forall l v syntax ann
         . ( Ord v
           , Eval v (Eff (Interpreter l v)) syntax
           , FreeVariables1 syntax
           , Functor syntax
           , MonadAddress l (Eff (Interpreter l v))
           , MonadPrim v (Eff (Interpreter l v))
           , Semigroup (Cell l v)
           )
         => Term syntax ann
         -> EvalResult l v
evaluate = run @(Interpreter l v) . fix ev pure


ev :: forall v w m syntax ann
   .  (FreeVariables1 syntax, Functor syntax, Eval v m syntax)
   => ((v -> m v) -> Term syntax ann -> m v)
   -> (v -> m w) -> Term syntax ann -> m w
ev ev' yield = eval ev' yield . unTerm

gc :: (Ord l, Foldable (Cell l), AbstractValue l a) => Set.Set (Address l a) -> Store l a -> Store l a
gc roots store = storeRestrict store (reachable roots store)

reachable :: (Ord l, Foldable (Cell l), AbstractValue l a) => Set.Set (Address l a) -> Store l a -> Set.Set (Address l a)
reachable roots store = go roots mempty
  where go set seen = case Set.minView set of
          Nothing -> seen
          Just (a, as)
            | Just values <- storeLookupAll a store -> go (Set.difference (foldr ((<>) . valueRoots) mempty values <> as) seen) (Set.insert a seen)
            | otherwise -> go seen (Set.insert a seen)

evCollect :: forall l t v m
          .  ( Ord l
             , Foldable (Cell l)
             , MonadStore l v m
             , MonadGC l v m
             , AbstractValue l v
             )
          => (Eval' t m v -> Eval' t m v)
          -> Eval' t m v
          -> Eval' t m v
evCollect ev0 ev' yield e = do
  roots <- askRoots :: m (Set.Set (Address l v))
  v <- ev0 ev' yield e
  modifyStore (gc (roots <> valueRoots v))
  return v

evRoots :: forall l v m syntax ann
        .  ( Ord l
           , MonadEnv l v m
           , MonadGC l v m
           , MonadPrim v m
           , AbstractValue l v
           , Eval v m (TermF syntax ann)
           , FreeVariables1 syntax
           , Functor syntax
           )
        => Eval' (Term syntax ann) m v
        -> Eval' (Term syntax ann) m v
evRoots ev' yield = eval ev' yield . unTerm
