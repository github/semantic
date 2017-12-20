module Data.Abstract.Type where

import Control.Monad.Fail
import Data.Traversable
import Prelude hiding (fail)

type TName = Int

data Type = Int | Bool | String | Unit | Type :-> Type | Type :* Type | TVar TName | TArr [Type]
  deriving (Eq, Ord, Show)

-- | Unify two 'Type's.
unify :: MonadFail m => Type -> Type -> m Type
unify Int  Int  = pure Int
unify Bool Bool = pure Bool
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify (a1 :* b1)  (a2 :* b2)  = (:*)  <$> unify a1 a2 <*> unify b1 b2
unify (TVar _) b = pure b
unify a (TVar _) = pure a
unify (TArr as) (TArr bs) = TArr <$> for (zip as bs) (uncurry unify)
unify t1 t2 = fail ("cannot unify " ++ show t1 ++ " with " ++ show t2)
