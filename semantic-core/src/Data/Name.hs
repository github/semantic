{-# LANGUAGE DeriveTraversable, ExistentialQuantification, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, OverloadedLists, OverloadedStrings, QuantifiedConstraints, RankNTypes, StandaloneDeriving, TypeOperators, UndecidableInstances #-}
module Data.Name
( User
, Namespaced
, Name(..)
, reservedNames
, isSimpleCharacter
, needsQuotation
, encloseIf
, Gensym(..)
, (//)
, gensym
, namespace
, Naming(..)
, runNaming
, NamingC(..)
, Incr(..)
, incr
, Scope(..)
, foldScope
, bind
, instantiate
) where

import           Control.Applicative
import           Control.Effect
import           Control.Effect.Carrier
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Sum
import           Control.Monad ((>=>))
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.Char as Char
import           Data.Function (on)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text as Text (Text, any, unpack)
import           Data.Text.Prettyprint.Doc (Pretty (..))
import qualified Data.Text.Prettyprint.Doc as Pretty

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

data Gensym
  = Root
  | Gensym :/ (Text, Int)
  deriving (Eq, Ord, Show)

instance Pretty Gensym where
  pretty = \case
    Root        -> pretty '◊'
    p :/ (n, x) -> Pretty.hcat [pretty p, "/", pretty n, "^", pretty x]

(//) :: Gensym -> Text -> Gensym
root // s = root :/ (s, 0)

infixl 6 //


gensym :: (Carrier sig m, Member Naming sig) => Text -> m Gensym
gensym s = send (Gensym s pure)

namespace :: (Carrier sig m, Member Naming sig) => Text -> m a -> m a
namespace s m = send (Namespace s m pure)


data Naming m k
  = Gensym Text (Gensym -> k)
  | forall a . Namespace Text (m a) (a -> k)

deriving instance Functor (Naming m)

instance HFunctor Naming where
  hmap _ (Gensym    s   k) = Gensym    s       k
  hmap f (Namespace s m k) = Namespace s (f m) k

instance Effect Naming where
  handle state handler (Gensym    s   k) = Gensym    s                        (handler . (<$ state) . k)
  handle state handler (Namespace s m k) = Namespace s (handler (m <$ state)) (handler . fmap k)


runNaming :: Functor m => NamingC m a -> m a
runNaming = runReader Root . evalState 0 . runNamingC

newtype NamingC m a = NamingC { runNamingC :: StateC Int (ReaderC Gensym m) a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Carrier sig m, Effect sig) => Carrier (Naming :+: sig) (NamingC m) where
  eff (L (Gensym    s   k)) = NamingC (StateC (\ i -> (:/ (s, i)) <$> ask >>= runState (succ i) . runNamingC . k))
  eff (L (Namespace s m k)) = NamingC (StateC (\ i -> local (// s) (evalState 0 (runNamingC m)) >>= runState i . runNamingC . k))
  eff (R other)             = NamingC (eff (R (R (handleCoercible other))))


data Incr a b
  = Z a
  | S b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Applicative (Incr a) where
  pure = S
  Z a <*> _ = Z a
  S f <*> a = f <$> a

instance Monad (Incr a) where
  Z a >>= _ = Z a
  S a >>= f = f a

match :: Applicative f => (b -> Maybe a) -> b -> Incr a (f b)
match f x | Just y <- f x = Z y
          | otherwise     = S (pure x)

fromIncr :: a -> Incr () a -> a
fromIncr a = incr (const a) id

incr :: (a -> c) -> (b -> c) -> Incr a b -> c
incr z s = \case { Z a -> z a ; S b -> s b }


newtype Scope a f b = Scope { unScope :: f (Incr a (f b)) }
  deriving (Foldable, Functor, Traversable)

instance (Eq   a, Eq   b, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Scope a f b) where
  (==) = (==) `on` (unScope >=> sequenceA)

instance (Ord  a, Ord  b, forall a . Eq   a => Eq   (f a)
                        , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Scope a f b) where
  compare = compare `on` (unScope >=> sequenceA)

deriving instance (Show a, Show b, forall a . Show a => Show (f a)) => Show (Scope a f b)

instance Applicative f => Applicative (Scope a f) where
  pure = Scope . pure . S . pure
  Scope f <*> Scope a = Scope (liftA2 (liftA2 (<*>)) f a)

instance Monad f => Monad (Scope a f) where
  Scope e >>= f = Scope (e >>= incr (pure . Z) (>>= unScope . f))

instance MonadTrans (Scope a) where
  lift = Scope . pure . S

foldScope :: (forall a . Incr z (n a) -> m (Incr z (n a)))
          -> (forall x y . (x -> m y) -> f x -> n y)
          -> (a -> m b)
          -> Scope z f a
          -> Scope z n b
foldScope k go h = Scope . go (k . fmap (go h)) . unScope

-- | Bind occurrences of a variable in a term, producing a term in which the variable is bound.
bind :: Applicative f => (b -> Maybe a) -> f b -> Scope a f b
bind name = Scope . fmap (match name) -- FIXME: succ as little of the expression as possible, cf https://twitter.com/ollfredo/status/1145776391826358273 — can this even be done generically?

-- | Substitute a term for the free variable in a given term, producing a closed term.
instantiate :: Monad f => f a -> Scope () f a -> f a
instantiate t = unScope >=> fromIncr t
