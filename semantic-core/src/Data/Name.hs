{-# LANGUAGE DeriveTraversable, LambdaCase, OverloadedLists #-}
module Data.Name
( User
, Named(..)
, named
, named'
, namedName
, namedValue
, Ignored(..)
, reservedNames
, isSimpleCharacter
, needsQuotation
) where

import qualified Data.Char as Char
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text as Text (Text, any, unpack)

-- | User-specified and -relevant names.
type User = Text

-- | Annotates an @a@ with a 'User'-provided name, which is ignored for '==' and 'compare'.
data Named a = Named (Ignored User) a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

named :: User -> a -> Named a
named = Named . Ignored

named' :: User -> Named User
named' u = Named (Ignored u) u

namedName :: Named a -> User
namedName (Named (Ignored n) _) = n

namedValue :: Named a -> a
namedValue (Named _ a) = a

newtype Ignored a = Ignored a
  deriving (Foldable, Functor, Show, Traversable)

instance Eq  (Ignored a) where _ == _ = True
instance Ord (Ignored a) where compare _ _ = EQ


reservedNames :: HashSet String
reservedNames = [ "#true", "#false", "if", "then", "else"
                , "#unit", "load", "rec", "#record"]

-- | Returns true if any character would require quotation or if the
-- name conflicts with a Core primitive.
needsQuotation :: User -> Bool
needsQuotation u = HashSet.member (unpack u) reservedNames || Text.any (not . isSimpleCharacter) u

-- | A ‘simple’ character is, loosely defined, a character that is compatible
-- with identifiers in most ASCII-oriented programming languages. This is defined
-- as the alphanumeric set plus @$@ and @_@.
isSimpleCharacter :: Char -> Bool
isSimpleCharacter = \case
  '$'  -> True -- common in JS
  '_'  -> True
  '?'  -> True -- common in Ruby
  c    -> Char.isAlphaNum c
