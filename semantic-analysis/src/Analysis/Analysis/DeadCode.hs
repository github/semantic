{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Analysis.DeadCode
( eval0
, subterms
, deadCodeFlowInsensitive
) where

import Analysis.Carrier.Fail.WithLoc
import qualified Analysis.Carrier.Store.Monovariant as A
import Analysis.Effect.Domain as A
import Analysis.Effect.Env
import qualified Analysis.Effect.Statement as S
import Analysis.Effect.Store
import Analysis.File
import Analysis.FlowInsensitive
import Analysis.Reference
import Analysis.Syntax hiding (eval0)
import Control.Applicative (Alternative(..))
import Control.Carrier.Fresh.Church
import Control.Carrier.Reader
import Control.Effect.Labelled
import Control.Effect.State
import Control.Monad.Trans.Class
import Data.Foldable (sequenceA_)
import Data.Function (fix)
import qualified Data.Set as Set

{-

- evaluator which first collects set of all subterms
- at each term evaluation, delete it from the set
- whatever is left is dead (unvisited)

-}

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m, Has (State (Set.Set Term)) sig m) => Term -> m val
eval0 term = do
  put (subterms term)
  let evalDead = \ eval' subterm -> do
        modify (Set.delete subterm)
        eval' subterm
  fix (eval . evalDead) term

subterms :: Term -> Set.Set Term
subterms t = Set.singleton t <> case t of
  Var _ -> mempty
  Noop -> mempty
  Iff c t e -> subterms c <> subterms t <> subterms e
  Bool _ -> mempty
  String _ -> mempty
  Throw t -> subterms t
  Let _ v b -> subterms v <> subterms b
  a :>> b -> subterms a <> subterms b
  Import _ -> mempty
  Function _ _ b -> subterms b
  Call f as -> subterms f <> foldMap subterms as
  Locate _ b -> subterms b


deadCodeFlowInsensitive
  :: Ord term
  => (forall sig m
     .  (Has (A.Dom Unit) sig m, Has (A.Env A.MAddr) sig m, Has (Reader Reference) sig m, HasLabelled A.Store (A.Store A.MAddr Unit) sig m, MonadFail m)
     => (term -> m Unit)
     -> (term -> m Unit)
     )
  -> [File term]
  -> ( A.MStore Unit
     , [File (Either (Reference, String) (Set.Set Unit))]
     )
deadCodeFlowInsensitive eval
  = run
  . evalFresh 0
  . A.runStoreState
  . traverse (runFile eval)

runFile
  :: ( Has Fresh sig m
     , Has (State (A.MStore Unit)) sig m
     , Ord term
     )
  => (forall sig m
     .  (Has (A.Dom Unit) sig m, Has (A.Env A.MAddr) sig m, Has (Reader Reference) sig m, HasLabelled A.Store (A.Store A.MAddr Unit) sig m, MonadFail m)
     => (term -> m Unit)
     -> (term -> m Unit)
     )
  -> File term
  -> m (File (Either (Reference, String) (Set.Set Unit)))
runFile eval file = traverse run file
  where run
          = runReader (fileRef file)
          . A.runEnv @Unit
          . runFail
          . convergeTerm (A.runStore @Unit . runDomain . fix (cacheTerm . eval))


data Unit = Unit
  deriving (Eq, Ord, Show)


newtype DomainC m a = DomainC { runDomain :: m a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

instance MonadTrans DomainC where
  lift = DomainC


instance ( Alternative m
         , Has (A.Env A.MAddr) sig m
         , Has Fresh sig m
         , HasLabelled A.Store (A.Store A.MAddr Unit) sig m
         , MonadFail m
         )
      => Algebra (A.Dom Unit :+: sig) (DomainC m) where
  alg hdl sig ctx = case sig of
    L (DVar _)  -> pure (Unit <$ ctx)

    L (DInt _)  -> pure (Unit <$ ctx)

    L DUnit     -> pure (Unit <$ ctx)

    L (DBool _) -> pure (Unit <$ ctx)
    L (DIf _ t e) -> hdl (t <$ ctx) <|> hdl (e <$ ctx)

    L (DString _) -> pure (Unit <$ ctx)

    L (DAbs n b) -> do
      addrs <- traverse A.alloc n
      let args = Unit <$ n
      sequenceA_ (zipWith (A..=) addrs args)
      hdl (b args <$ ctx)
    L (DApp _ _) -> pure (Unit <$ ctx)

    L (_ :>>> t) -> pure (t <$ ctx)

    L (DDie msg) -> fail (show msg)

    R other -> DomainC (alg (runDomain . hdl) other ctx)
