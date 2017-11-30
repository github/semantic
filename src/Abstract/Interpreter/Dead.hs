{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Abstract.Interpreter.Dead where

import Abstract.Interpreter
import Analysis.Abstract.Eval
import Control.Effect
import Control.Monad.Effect hiding (run)
import Control.Monad.Effect.Dead
import Control.Monad.Effect.State
import Control.Monad.Effect.Store
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Data.Function (fix)
import Data.Functor.Foldable
import Data.Functor.Classes
import Data.Pointed
import Data.Semigroup
import Data.Set
import Data.Term


type DeadCodeInterpreter t v = State (Dead t) ': Interpreter v

type DeadCodeResult t v = Final (DeadCodeInterpreter t v) v


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
