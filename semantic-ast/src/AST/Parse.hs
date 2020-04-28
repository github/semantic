{-# LANGUAGE DataKinds, DeriveGeneric, DeriveTraversable, KindSignatures #-}
module AST.Parse
( Err(..)
) where

import GHC.Generics (Generic, Generic1)

-- | An AST node representing an Error, showing a parse that's succeeded or failed.
--
-- Error types are isomorphic to Either String. 
--
-- For example, consider the following:
-- @
-- data If f a = If { ann :: a, condition :: f (Expr f a), consequence :: f (Stmt f a), alternative :: f (Stmt f a) }
-- @
-- When the parse fails, the f will be substituted with Err
data Err a = Fail String | Succeed a
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Traversable)

instance Applicative Err where
    pure            = Succeed
    Fail e <*> _    = Fail e
    Succeed a <*> r = fmap a r

instance Show a => Show (Err a) where
  show (Fail msg)  = msg 
  show (Succeed a) = show a
