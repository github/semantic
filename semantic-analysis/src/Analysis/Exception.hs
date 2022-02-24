{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Exception
( Exception(..)
, ExcSet(..)
, var
, exc
  -- * Exception tracing analysis
, ExcC(..)
) where

import           Analysis.Effect.Domain
import           Analysis.Name
import           Control.Algebra
import           Control.Applicative (Alternative (..))
import qualified Data.Set as Set

newtype Exception = Exception { exceptionName :: String }
  deriving (Eq, Ord, Show)

-- | Sets whose elements are each a variable or an exception.
data ExcSet = ExcSet { freeVariables :: Set.Set Name, exceptions :: Set.Set Exception }
  deriving (Eq, Ord, Show)

instance Semigroup ExcSet where
  ExcSet v1 e1 <> ExcSet v2 e2 = ExcSet (v1 <> v2) (e1 <> e2)

instance Monoid ExcSet where
  mempty = ExcSet mempty mempty

var :: Name -> ExcSet
var v = ExcSet (Set.singleton v) mempty

exc :: Exception -> ExcSet
exc e = ExcSet mempty (Set.singleton e)


newtype ExcC m a = ExcC { runExcC :: m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Algebra sig m, Alternative m) => Algebra (Dom ExcSet :+: sig) (ExcC m) where
  alg hdl sig ctx = ExcC $ case sig of
    L dom   -> case dom of
      DVar n    -> pure $ var n <$ ctx
      DAbs _ b  -> runExcC (hdl (b mempty <$ ctx))
      DApp f a  -> pure $ f <> a <$ ctx
      DInt _    -> pure nil
      DUnit     -> pure nil
      DBool _   -> pure nil
      DIf c t e -> fmap (mappend c) <$> runExcC (hdl (t <$ ctx) <|> hdl (e <$ ctx))
      DString _ -> pure nil
      -- FIXME: return a set indicating a failure with e as the payload
      DDie e    -> pure $ e <$ ctx
      where
      nil = (mempty :: ExcSet) <$ ctx

    R other -> alg (runExcC . hdl) other ctx
