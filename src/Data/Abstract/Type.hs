module Data.Abstract.Type where

import Control.Monad.Fail
import Data.Traversable
import Prelude hiding (fail)

-- | The type of type variable names.
type TName = Int

-- | A datatype representing primitive types and combinations thereof.
data Type
  = Int            -- ^ Primitive int type.
  | Bool           -- ^ Primitive boolean type.
  | String         -- ^ Primitive string type.
  | Unit           -- ^ The unit type.
  | Type :-> Type  -- ^ Binary function types.
  | TVar TName     -- ^ A type variable.
  | Product [Type] -- ^ N-ary products.
  deriving (Eq, Ord, Show)

-- TODO: À la carte representation of types.


-- | Unify two 'Type's.
unify :: MonadFail m => Type -> Type -> m Type
unify Int  Int  = pure Int
unify Bool Bool = pure Bool
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
unify (TVar _) b = pure b
unify a (TVar _) = pure a
-- FIXME: this can succeed incorrectly for lists of inequal length.
unify (Product as) (Product bs) = Product <$> for (zip as bs) (uncurry unify)
unify t1 t2 = fail ("cannot unify " ++ show t1 ++ " with " ++ show t2)
