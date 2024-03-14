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
( deadCodeFlowInsensitive
) where

import           Analysis.Carrier.Fail.WithLoc
import qualified Analysis.Carrier.Statement.State as A
import qualified Analysis.Carrier.Store.Monovariant as A
import           Analysis.Effect.Domain as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Reference
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Fresh.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Church
import           Control.Effect.Labelled
import           Control.Monad (zipWithM_)
import           Control.Monad.Trans.Class
import           Data.Function (fix)
import qualified Data.Set as Set

deadCodeFlowInsensitive
  :: Ord term
  => (forall sig m
     .  (Has (A.Dom Unit) sig m, Has (A.Env A.MAddr) sig m, Has (Reader Reference) sig m, Has A.Statement sig m, HasLabelled A.Store (A.Store A.MAddr Unit) sig m, MonadFail m)
     => (term -> m Unit)
     -> (term -> m Unit)
     )
  -> (term -> Set.Set term)
  -> [File term]
  -> ( Set.Set term
     , A.MStore Unit
     , [File (Either (Reference, String) (Set.Set Unit))]
     )
deadCodeFlowInsensitive eval subterms
  = run
  . runState (\ dead (store, files) -> pure (dead, store, files)) Set.empty
  . evalFresh 0
  . A.runStoreState
  . traverse (runFile eval subterms)

runFile
  :: ( Has Fresh sig m
     , Has (State (A.MStore Unit)) sig m
     , Has (State (Set.Set term)) sig m
     , Ord term
     )
  => (forall sig m
     .  (Has (A.Dom Unit) sig m, Has (A.Env A.MAddr) sig m, Has (Reader Reference) sig m, Has A.Statement sig m, HasLabelled A.Store (A.Store A.MAddr Unit) sig m, MonadFail m)
     => (term -> m Unit)
     -> (term -> m Unit)
     )
  -> (term -> Set.Set term)
  -> File term
  -> m (File (Either (Reference, String) (Set.Set Unit)))
runFile eval subterms file = traverse run file
  where run term = do
          modify (<> subterms term)
          A.runStatement (const pure)
            . runReader (fileRef file)
            . A.runEnv @Unit
            . runFail
            . convergeTerm (A.runStore @Unit . runDomain . fix (cacheTerm . evalDead))
            $ term
        evalDead eval' subterm = do
          modify (Set.delete subterm)
          eval eval' subterm


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
      zipWithM_ (A..=) addrs args
      hdl (b args <$ ctx)
    L (DApp _ _) -> pure (Unit <$ ctx)

    L (_ :>>> t) -> pure (t <$ ctx)

    L (DDie msg) -> fail (show msg)

    R other -> DomainC (alg (runDomain . hdl) other ctx)
