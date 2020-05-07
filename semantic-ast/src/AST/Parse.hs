{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE  KindSignatures #-}

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
-- TODO: this could work with AST.Element Prj given the kindedness was adjusted from (*) to (* -> *).
data Err a = Fail String | Success a
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Traversable)

instance Applicative Err where
    pure            = Success
    Fail e <*> _    = Fail e
    Success a <*> r = fmap a r

instance Show a => Show (Err a) where
  show (Fail msg)  = msg 
  show (Success a) = show a
