module Data.Abstract.Type where

import Prologue
import Data.Align (alignWith)
import Prelude hiding (fail)

-- | The type of type variable names.
type TName = Int

-- | A datatype representing primitive types and combinations thereof.
data Type
  = Int            -- ^ Primitive int type.
  | Bool           -- ^ Primitive boolean type.
  | String         -- ^ Primitive string type.
  | Unit           -- ^ The unit type.
  | Float          -- ^ Floating-point type.
  | Rational       -- ^ Rational type.
  | Type :-> Type  -- ^ Binary function types.
  | Var TName      -- ^ A type variable.
  | Product [Type] -- ^ N-ary products.
  deriving (Eq, Ord, Show)

-- TODO: À la carte representation of types.


-- | Unify two 'Type's.
unify :: MonadFail m => Type -> Type -> m Type
unify (a1 :-> b1) (a2 :-> b2) = (:->) <$> unify a1 a2 <*> unify b1 b2
-- FIXME: this should be constructing a substitution.
unify (Var _) b = pure b
unify a (Var _) = pure a
unify (Product as) (Product bs) = Product <$> sequenceA (alignWith (these pure pure unify) as bs)
unify t1 t2
  | t1 == t2  = pure t2
  | otherwise = fail ("cannot unify " ++ show t1 ++ " with " ++ show t2)
