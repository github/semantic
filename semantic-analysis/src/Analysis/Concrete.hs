{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Concrete
( Concrete(..)
, concrete
) where

import qualified Analysis.Carrier.Env.Precise as A
import           Analysis.Carrier.Fail.WithLoc
import qualified Analysis.Carrier.Heap.Precise as A
import           Analysis.Effect.Domain as A
import           Analysis.File
import           Analysis.Functor.Named
import           Analysis.Reference
import           Control.Algebra
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader hiding (Local)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Data.Foldable (foldl')
import           Data.Function (fix, on)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import           Data.Semigroup (Last(..))
import           Data.Text as Text (Text, pack)
import           Prelude hiding (fail)
import           Source.Span
import qualified System.Path as Path

type Addr = Int
type Env = Map.Map Name Addr

data Concrete
  = Closure Path.AbsRelFile Span (Named (Concrete -> Concrete))
  | Unit
  | Bool Bool
  | Int Int
  | String Text
  | Neutral Name (Snoc (Elim Concrete))
  -- NB: We derive the 'Semigroup' instance for 'Concrete' to take the second argument. This is equivalent to stating that the return value of an imperative sequence of statements is the value of its final statement.
  deriving Semigroup via Last Concrete

instance Eq Concrete where
  (==) = (==) `on` quote

instance Ord Concrete where
  compare = compare `on` quote

instance Show Concrete where
  showsPrec p = showsPrec p . quote


data Snoc a = Nil | Snoc a :> a
  deriving (Foldable, Functor, Traversable)


newtype Elim a = EApp a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

vvar :: Name -> Concrete
vvar n = Neutral n Nil

velim :: Concrete -> Elim Concrete -> Concrete
velim (Closure _ _ f) (EApp a) = namedValue f a
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
    Closure p s b -> Closure p s ((go .) <$> b) -- NB: Shadowing can’t happen because n' can’t occur inside b
    Unit          -> Unit
    Bool b        -> Bool b
    Int i         -> Int i
    String s      -> String s


data FO
  = FOVar Name
  | FOClosure Path.AbsRelFile Span (Named FO)
  | FOUnit
  | FOBool Bool
  | FOInt Int
  | FOString Text
  | FOApp FO FO
  deriving (Eq, Ord, Show)


quote :: Concrete -> FO
quote = \case
-- FIXME: should quote take a Level incremented under binders?
  Closure path span body -> FOClosure path span ((\ f -> quote (f (vvar (namedName body)))) <$> body)
  Unit                   -> FOUnit
  Bool b                 -> FOBool b
  Int i                  -> FOInt i
  String s               -> FOString s
  Neutral n sp           -> foldl' (\ f (EApp a) -> FOApp f (quote a)) (FOVar n) sp


type Heap = IntMap.IntMap

type Eval term m value = (term -> m value) -> (term -> m value)


concrete
  :: (forall sig m
     .  (Has (A.Dom Concrete :+: A.Env Addr :+: A.Heap Addr Concrete :+: Reader Reference) sig m, MonadFail m)
     => Eval term m Concrete
     )
  -> [File term]
  -> (Heap Concrete, [File (Either (Reference, String) Concrete)])
concrete eval
  = run
  . evalFresh 0
  . A.runHeap
  . traverse (runFile eval)

runFile
  :: ( Has Fresh sig m
     , Has (A.Heap Addr Concrete) sig m
     )
  => (forall sig m
     .  (Has (A.Dom Concrete :+: A.Env Addr :+: A.Heap Addr Concrete :+: Reader Reference) sig m, MonadFail m)
     => Eval term m Concrete
     )
  -> File term
  -> m (File (Either (Reference, String) Concrete))
runFile eval file = traverse run file
  where run = runReader (fileRef file)
            . runFail
            . runReader @Env mempty
            . A.runEnv
            . runDomain
            . fix eval


newtype DomainC m a = DomainC { runDomain :: m a }
  deriving (Applicative, Functor, Monad, MonadFail)

instance MonadTrans DomainC where
  lift = DomainC

instance ( Has (A.Env Addr) sig m
         , Has (A.Heap Addr Concrete) sig m
         , Has (Reader Reference) sig m
         , MonadFail m
         )
      => Algebra (A.Dom Concrete :+: sig) (DomainC m) where
  alg hdl sig ctx = case sig of
    L (DVar n)    -> pure (vvar n <$ ctx)
    L (DAbs n b)  -> do
      b' <- hdl (b (vvar n) <$ ctx)
      ref <- ask
      pure $ Closure (refPath ref) (refSpan ref) . named n . flip (vsubst n) <$> b'
    L (DApp f a)  -> pure $ velim f (EApp a) <$ ctx
    L (DInt i)    -> pure (Int i <$ ctx)
    L DUnit       -> pure (Unit <$ ctx)
    L (DBool b)   -> pure (Bool b <$ ctx)
    L (DIf c t e) -> case c of
      Bool b
        | b         -> hdl (t <$ ctx)
        | otherwise -> hdl (e <$ ctx)
      _             -> fail "expected Bool"
    L (DString s) -> pure (String (Text.pack s) <$ ctx)
    L (DDie msg)  -> fail msg

    R other       -> DomainC (alg (runDomain . hdl) other ctx)
