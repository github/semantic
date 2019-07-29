{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, OverloadedLists, OverloadedStrings, StandaloneDeriving, TypeApplications, TypeOperators, UndecidableInstances #-}
module Data.Name
( User
, Namespaced
, Name(..)
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
, prime
, fresh
, namespace
, Naming(..)
, runNaming
, NamingC(..)
) where

import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad.Fail
import           Control.Monad.IO.Class
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

data Name
  -- | A locally-bound, machine-generatable name.
  --
  --   This should be used for locals, function parameters, and similar names which can’t escape their defining scope.
  = Gen Gensym
  -- | A name provided by a user.
  --
  --   This should be used for names which the user provided and which other code (other functions, other modules, other packages) could call, e.g. declaration names.
  | User User
  deriving (Eq, Ord, Show)

instance Pretty Name where
  pretty = \case
    Gen p  -> pretty p
    User n -> pretty n


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

prime :: Gensym -> Gensym
prime (Gensym s i) = Gensym s (succ i)


fresh :: (Carrier sig m, Member Naming sig) => m Gensym
fresh = send (Fresh pure)

namespace :: (Carrier sig m, Member Naming sig) => Text -> m a -> m a
namespace s m = send (Namespace s m pure)


data Naming m k
  = Fresh (Gensym -> m k)
  | forall a . Namespace Text (m a) (a -> m k)

deriving instance Functor m => Functor (Naming m)

instance HFunctor Naming where
  hmap f (Fresh         k) = Fresh             (f . k)
  hmap f (Namespace s m k) = Namespace s (f m) (f . k)

instance Effect Naming where
  handle state handler (Fresh         k) = Fresh                              (handler . (<$ state) . k)
  handle state handler (Namespace s m k) = Namespace s (handler (m <$ state)) (handler . fmap k)


runNaming :: Functor m => NamingC m a -> m a
runNaming = runReader Nil . evalState 0 . runNamingC

newtype NamingC m a = NamingC { runNamingC :: StateC Int (ReaderC (Stack Text) m) a }
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Carrier sig m, Effect sig) => Carrier (Naming :+: sig) (NamingC m) where
  eff (L (Fresh         k)) = NamingC (asks Gensym <*> get <* modify (succ @Int) >>= runNamingC . k)
  eff (L (Namespace s m k)) = NamingC (StateC (\ i -> local (:> s) (evalState 0 (runNamingC m)) >>= runState i . runNamingC . k))
  eff (R other)             = NamingC (eff (R (R (handleCoercible other))))
