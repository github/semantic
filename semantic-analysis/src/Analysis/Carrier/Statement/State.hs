{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Carrier.Statement.State
( -- * Messages
  Message(..)
  -- * Statement carrier
, runStatement
, StatementC(..)
  -- * Statement effect
, module Analysis.Effect.Statement
) where

import           Analysis.Effect.Statement hiding (Import)
import qualified Analysis.Effect.Statement as S
import           Control.Algebra
import           Control.Carrier.State.Church
import           Control.Monad.Fail as Fail
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)

-- Messages

newtype Message
  = Import (NonEmpty Text)
  deriving (Eq, Ord, Show)


-- Statement carrier

runStatement :: ([Message] -> a -> m r) -> StatementC m a -> m r
runStatement k (StatementC m) = runState (k . reverse) [] m

newtype StatementC m a = StatementC { runStatementC :: StateC [Message] m a }
  deriving (Applicative, Functor, Monad, Fail.MonadFail)

instance Algebra sig m => Algebra (S.Statement :+: sig) (StatementC m) where
  alg hdl sig ctx = case sig of
    L (S.Import ns) -> StatementC ((<$ ctx) <$> modify (Import ns:))
    R other         -> StatementC (alg (runStatementC . hdl) (R other) ctx)
