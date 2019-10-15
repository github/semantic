{-# LANGUAGE DeriveGeneric, DeriveTraversable, GeneralizedNewtypeDeriving, LambdaCase, OverloadedLists #-}
module Core.Name
( Name (..)
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
import           Data.String (IsString)
import           Data.Text as Text (Text, any, unpack)
import           Data.Text.Prettyprint.Doc (Pretty)
import           GHC.Generics (Generic)

-- | User-specified and -relevant names.
newtype Name = Name { unName :: Text }
  deriving (Eq, Generic, IsString, Ord, Pretty, Show)

-- | Annotates an @a@ with a 'Name'-provided name, which is ignored for '==' and 'compare'.
data Named a = Named (Ignored Name) a
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

named :: Name -> a -> Named a
named = Named . Ignored

named' :: Name -> Named Name
named' u = Named (Ignored u) u

namedName :: Named a -> Name
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
needsQuotation :: Name -> Bool
needsQuotation (Name u) = HashSet.member (unpack u) reservedNames || Text.any (not . isSimpleCharacter) u

-- | A ‘simple’ character is, loosely defined, a character that is compatible
-- with identifiers in most ASCII-oriented programming languages. This is defined
-- as the alphanumeric set plus @$@ and @_@.
isSimpleCharacter :: Char -> Bool
isSimpleCharacter = \case
  '$'  -> True -- common in JS
  '_'  -> True
  '?'  -> True -- common in Ruby
  c    -> Char.isAlphaNum c
