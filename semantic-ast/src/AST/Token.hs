{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE KindSignatures #-}
module AST.Token
( Token(..)
) where

import Data.Aeson
import GHC.Generics (Generic, Generic1)
import GHC.TypeLits (Nat, Symbol)

-- | An AST node representing a token, indexed by its name and numeric value.
--
-- For convenience, token types are typically used via type synonyms, e.g.:
--
-- @
-- type AnonymousPlus = Token "+" 123
-- @
newtype Token (symName :: Symbol) (symVal :: Nat) a = Token { ann :: a }
  deriving (Eq, Foldable, Functor, Generic, Generic1, Ord, Show, Traversable)

instance ToJSON1 (Token symName val)
