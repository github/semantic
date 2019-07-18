{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, OverloadedLists, OverloadedStrings, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Data.Name
( User
, Namespaced
, Named(..)
, named
, named'
, namedName
, namedValue
, Ignored(..)
, reservedNames
, isSimpleCharacter
, needsQuotation
, encloseIf
, Gensym(..)
) where

import qualified Data.Char as Char
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Stack
import           Data.Text as Text (Text, any, unpack)
import           Data.Text.Prettyprint.Doc (Pretty (..))

-- | User-specified and -relevant names.
type User = Text

-- | The type of namespaced actions, i.e. actions occurring within some outer name.
--
--   This corresponds to the @Agent@ type synonym described in /I Am Not a Number—I Am a Free Variable/.
type Namespaced a = Gensym -> a

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
reservedNames = [ "#true", "#false", "let", "#frame", "if", "then", "else"
                , "lexical", "import", "#unit", "load"]

-- | Returns true if any character would require quotation or if the
-- name conflicts with a Core primitive.
needsQuotation :: User -> Bool
needsQuotation u = HashSet.member (unpack u) reservedNames || Text.any (not . isSimpleCharacter) u

encloseIf :: Monoid m => Bool -> m -> m -> m -> m
encloseIf True  l r x = l <> x <> r
encloseIf False _ _ x = x

-- | A ‘simple’ character is, loosely defined, a character that is compatible
-- with identifiers in most ASCII-oriented programming languages. This is defined
-- as the alphanumeric set plus @$@ and @_@.
isSimpleCharacter :: Char -> Bool
isSimpleCharacter = \case
  '$'  -> True -- common in JS
  '_'  -> True
  '?'  -> True -- common in Ruby
  c    -> Char.isAlphaNum c


data Gensym = Gensym (Stack Text) Int
  deriving (Eq, Ord, Show)

instance Pretty Gensym where
  pretty (Gensym _ i) = pretty (alphabet !! r : if q > 0 then show q else "")
    where (q, r) = i `divMod` 26
          alphabet = ['a'..'z']
