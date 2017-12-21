{-# LANGUAGE DefaultSignatures, MultiParamTypeClasses, ScopedTypeVariables, TypeOperators, UndecidableInstances #-}
module Data.Abstract.Eval
( Eval(..)
, MonadGC(..)
, MonadFail(..)
) where

import Control.Monad.Effect.Env
import Control.Monad.Effect.GC
import Control.Monad.Fail
import Data.Abstract.Environment
import Data.Abstract.FreeVariables
import Data.Abstract.Value
import Data.Functor.Classes
import Data.Proxy
import Data.Term
import Data.Union
import Prelude hiding (fail)


-- | The 'Eval' class defines the necessary interface for a term to be evaluated. While a default definition of 'eval' is given, instances with computational content must implement 'eval' to perform their small-step operational semantics.
class Monad m => Eval term v m constr where
  -- | Evaluate a term using an open-recursive evaluator for any child terms.
  eval :: ((v -> m v) -> term -> m v) -- ^ The “recur” function. An open-recursive evaluator for child terms, taking a continuation representing the remainder of the child’s scope. Syntax representing imperative sequences of statements should pass in a continuation evaluating the remaining statements. Syntax which introduces a lexical scope for variable bindings should pass 'pure'. Syntax which does not delimit variable bindings should pass the continuation that it itself was passed by its parent.
       -> (v -> m v)                  -- ^ The “yield” function. A continuation representing the remainder of the current (imperative) scope. This allows each statement in an imperative sequence to affect the environment of later statements in the same scope, but without allowing such effects to escape their scope. For example, @do { x <- getX ; f x }@ binds @x@ in the local environment in the first statement s.t. the second can use it, but remains unbound outside of the @do@-block.
       -> constr term                 -- ^ The current instruction in a program.
       -> m v                         -- ^ A monadic computation producing the (abstract) evaluation of the current instruction.

  default eval :: (MonadFail m, Show1 constr) => ((v -> m v) -> term -> m v) -> ((v -> m v) -> constr term -> m v)
  eval _ _ expr = fail $ "Eval unspecialized for " ++ liftShowsPrec (const (const id)) (const id) 0 expr ""

instance (Monad m, Apply (Eval t v m) fs) => Eval t v m (Union fs) where
  eval ev yield = apply (Proxy :: Proxy (Eval t v m)) (eval ev yield)

instance (Monad m, Eval t v m s) => Eval t v m (TermF s a) where
  eval ev yield In{..} = eval ev yield termFOut


instance ( Monad m
         , Ord (LocationFor v)
         , MonadGC v m
         , MonadEnv v m
         , AbstractValue v
         , FreeVariables t
         )
         => Eval t v m [] where
  eval _  yield []     = yield unit
  eval ev yield [a]    = ev pure a >>= yield
  eval ev yield (a:as) = do
    env <- askEnv :: m (Environment (LocationFor v) v)
    extraRoots (envRoots env (freeVariables1 as)) (ev (const (eval ev pure as)) a) >>= yield
