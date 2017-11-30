{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Dead where

import Abstract.Eval
import Abstract.FreeVariables
import Abstract.Interpreter
import Abstract.Store
import Abstract.Value

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup
import Data.Set
import Data.Term


type DeadCodeInterpreter t v = State (Dead t) ': Interpreter v

type DeadCodeResult t v = Final (DeadCodeInterpreter t v) v


newtype Dead a = Dead { unDead :: Set a }
  deriving (Eq, Foldable, Semigroup, Monoid, Ord, Show)


class Monad m => MonadDead t m where
  killAll :: Dead t -> m ()
  revive :: Ord t => t -> m ()

instance (State (Dead t) :< fs) => MonadDead t (Eff fs) where
  killAll = put
  revive = modify . (Dead .) . (. unDead) . delete


subterms :: (Ord a, Recursive a, Foldable (Base a)) => a -> Set a
subterms term = para (foldMap (uncurry ((<>) . point))) term <> point term


-- Dead code analysis
--
-- Example:
--    evalDead @(Value Syntax Precise) <term>

evalDead :: forall v syntax ann
         . ( Ord v
           , Ord ann
           , Ord1 syntax
           , Recursive (Term syntax ann)
           , Foldable syntax
           , FreeVariables1 syntax
           , Functor syntax
           , Eval (Term syntax ann) v (Eff (DeadCodeInterpreter (Term syntax ann) v)) syntax
           , MonadAddress (LocationFor v) (Eff (DeadCodeInterpreter (Term syntax ann) v))
           , Semigroup (Cell (LocationFor v) v)
           )
         => Term syntax ann
         -> DeadCodeResult (Term syntax ann) v
evalDead e0 = run @(DeadCodeInterpreter (Term syntax ann) v) $ do
  killAll (Dead (subterms e0))
  fix (evDead ev) pure e0

evDead :: (Ord t, MonadDead t m)
       => (Eval' t m v -> Eval' t m v)
       -> Eval' t m v
       -> Eval' t m v
evDead ev0 ev' yield e = do
  revive e
  ev0 ev' yield e
