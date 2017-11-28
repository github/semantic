{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DeriveFoldable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Abstract.Interpreter.Dead where

import Abstract.Eval
import Abstract.Interpreter
import Abstract.Primitive
import Abstract.Set
import Abstract.Store
import Data.Term

import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.State
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup


type DeadCodeInterpreter l t v = State (Dead t) ': Interpreter l v

type DeadCodeResult l t v = Final (DeadCodeInterpreter l t v) v


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
-- Example:
--    evalDead @Precise @(Value Syntax Precise) @Syntax (if' true (Abstract.Syntax.int 1) (Abstract.Syntax.int 2))
evalDead :: forall l v syntax ann
         . ( Ord v
           , Ord ann
           , Ord1 syntax
           , Recursive (Term syntax ann)
           , Foldable (Base (Term syntax ann))
           , Eval l v (Eff (DeadCodeInterpreter l (Term syntax ann) v)) syntax ann syntax
           , MonadAddress l (Eff (DeadCodeInterpreter l (Term syntax ann) v))
           , MonadPrim v (Eff (DeadCodeInterpreter l (Term syntax ann) v))
           , Semigroup (Cell l v)
           )
         => Term syntax ann
         -> DeadCodeResult l (Term syntax ann) v
evalDead e0 = run @(DeadCodeInterpreter l (Term syntax ann) v) $ do
  killAll (Dead (subterms e0))
  fix (evDead (ev @l)) pure e0

evDead :: (Ord t, MonadDead t m)
       => (Eval' t m v -> Eval' t m v)
       -> Eval' t m v
       -> Eval' t m v
evDead ev0 ev yield e = do
  revive e
  ev0 ev yield e
