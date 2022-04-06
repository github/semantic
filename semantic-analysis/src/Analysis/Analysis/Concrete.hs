{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Analysis.Concrete
( Concrete(..)
, concrete
) where

import           Analysis.Carrier.Fail.WithLoc
import qualified Analysis.Carrier.Store.Precise as A
import           Analysis.Data.Snoc
import           Analysis.Effect.Domain as A
import           Analysis.File
import           Analysis.Functor.Named
import           Analysis.Reference
import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader hiding (Local)
import           Control.Effect.Labelled
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Data.Foldable (foldl')
import           Data.Function (fix, on)
import           Data.Semigroup (Last (..))
import           Data.Text as Text (Text)
import           Prelude hiding (fail)

data Concrete
  = Closure Reference [Name] ([Concrete] -> Concrete)
  | Unit
  | Bool Bool
  | Int Int
  | String Text
  -- | A neutral value, consisting of a variable at the head, followed by a “spine” of eliminations.
  --
  -- This constructor is key to hereditary substitution, whereby a value in 'Concrete' is a normal form by virtue of either being a value (i.e. one of the other constructors) or 'Neutral', somewhere inside a 'Closure'. In the latter case, when the surrounding closure is eliminated, if it substitutes out the variable at the head, the eliminations are applied, immediately reducing it. Since eliminations can’t be applied to any of the other constructors, there’s no way to represent redexes, so we’re left with a new term which is itself either already a value, or else is a neutral term underneath a closure.
  --
  -- This is essential for parametric modular analysis, as it allows us to obtain the same benefits not just for variables inside closures, but also variables which aren’t known from the surrounding (local) context (e.g. as-yet-unresolved imports). That in turn allows analysis to be performed 1. before we’ve computed an import graph, which is particularly difficult in some languages, 2. completely in parallel, and 3. incrementally.
  | Neutral Name (Snoc (Elim Concrete))
  -- NB: We derive the 'Semigroup' instance for 'Concrete' to take the second argument. This is equivalent to stating that the return value of an imperative sequence of statements is the value of its final statement.
  deriving Semigroup via Last Concrete

instance Eq Concrete where
  (==) = (==) `on` quote

instance Ord Concrete where
  compare = compare `on` quote

instance Show Concrete where
  showsPrec p = showsPrec p . quote


newtype Elim a = EApp [a]
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

vvar :: Name -> Concrete
vvar n = Neutral n Nil

velim :: Concrete -> Elim Concrete -> Concrete
velim (Closure _ _ f) (EApp a) = f a
velim (Neutral h as)  a        = Neutral h (as :> a)
velim _               _        = error "velim: cannot eliminate" -- FIXME: fail in the monad instead

-- FIXME: so why use HOAS at all then?
-- FIXME: construct in de Bruijn-indexed rep & simple eval instead of substituting.
vsubst :: Name -> Concrete -> Concrete -> Concrete
vsubst n v = go
  where
  go = \case
    Neutral n' as
      | n == n'   -> foldl' velim v as'
      | otherwise -> Neutral n'     as'
      where
      as' = fmap go <$> as
    Closure r n b -> Closure r n (go . b) -- NB: Shadowing can’t happen because n' can’t occur inside b
    Unit          -> Unit
    Bool b        -> Bool b
    Int i         -> Int i
    String s      -> String s


data FO
  = FOVar Name
  | FOClosure Reference [Name] FO
  | FOUnit
  | FOBool Bool
  | FOInt Int
  | FOString Text
  | FOApp FO [FO]
  deriving (Eq, Ord, Show)


quote :: Concrete -> FO
quote = \case
-- FIXME: should quote take a Level incremented under binders?
  Closure r n body -> FOClosure r n (quote (body (map vvar n)))
  Unit             -> FOUnit
  Bool b           -> FOBool b
  Int i            -> FOInt i
  String s         -> FOString s
  Neutral n sp     -> foldl' (\ f (EApp a) -> FOApp f (map quote a)) (FOVar n) sp


type Eval term m value = (term -> m value) -> (term -> m value)


concrete
  :: (forall sig m
     .  (Has (A.Dom Concrete :+: A.Env A.PAddr :+: Reader Reference) sig m, HasLabelled A.Store (A.Store A.PAddr Concrete) sig m, MonadFail m)
     => Eval term m Concrete
     )
  -> [File term]
  -> (A.PStore Concrete, [File (Either (Reference, String) Concrete)])
concrete eval
  = run
  . evalFresh 0
  . A.runStore
  . traverse (runFile eval)

runFile
  :: HasLabelled A.Store (A.Store A.PAddr Concrete) sig m
  => (forall sig m
     .  (Has (A.Dom Concrete :+: A.Env A.PAddr :+: Reader Reference) sig m, HasLabelled A.Store (A.Store A.PAddr Concrete) sig m, MonadFail m)
     => Eval term m Concrete
     )
  -> File term
  -> m (File (Either (Reference, String) Concrete))
runFile eval file = traverse run file
  where run = runReader (fileRef file)
            . runFail
            . A.runEnv
            . runDomain
            . fix eval


newtype DomainC m a = DomainC { runDomain :: m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance MonadTrans DomainC where
  lift = DomainC

instance ( Has (A.Env A.PAddr) sig m
         , HasLabelled A.Store (A.Store A.PAddr Concrete) sig m
         , Has (Reader Reference) sig m
         , MonadFail m
         )
      => Algebra (A.Dom Concrete :+: sig) (DomainC m) where
  alg hdl sig ctx = case sig of
    L (DVar n)    -> pure (vvar n <$ ctx)
    L (DAbs n b)  -> do
      b' <- hdl (b (map vvar n) <$ ctx)
      ref <- ask
      let closure body = Closure ref n (\ args -> let substs = zipWith vsubst n args in foldl' (.) id substs body)
      pure $ closure <$> b'
    L (DApp f a)  -> pure $ velim f (EApp a) <$ ctx
    L (DInt i)    -> pure (Int i <$ ctx)
    L DUnit       -> pure (Unit <$ ctx)
    L (DBool b)   -> pure (Bool b <$ ctx)
    L (DIf c t e) -> case c of
      Bool b
        | b         -> hdl (t <$ ctx)
        | otherwise -> hdl (e <$ ctx)
      _             -> fail "expected Bool"
    L (DString s) -> pure (String s <$ ctx)
    L (DDie msg)  -> fail (show (quote msg))

    R other       -> DomainC (alg (runDomain . hdl) other ctx)
