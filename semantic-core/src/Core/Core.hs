{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Core.Core
( Core(..)
, rec
, (>>>)
, unseq
, unseqs
, (>>>=)
, unbind
, unstatement
, do'
, unstatements
, (:<-)(..)
, lam
, lams
, unlam
, ($$)
, ($$*)
, unapply
, unapplies
, unit
, bool
, if'
, string
, load
, record
, (...)
, (.?)
, (.=)
, Ann(..)
, ann
, annAt
, annWith
, instantiate
, stripAnnotations
) where

import Control.Algebra
import Control.Applicative (Alternative (..))
import Core.Name
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import GHC.Generics (Generic1)
import GHC.Stack
import Source.Span
import Syntax.Foldable
import Syntax.Module
import Syntax.Scope
import Syntax.Stack
import Syntax.Sum
import Syntax.Term
import Syntax.Traversable

data Core f a
  -- | Recursive local binding of a name in a scope; strict evaluation of the name in the body will diverge.
  --
  --   Simultaneous (and therefore potentially mutually-recursive) bindings can be made by binding a 'Record' recursively within 'Rec' and projecting from it with ':.'.
  = Rec (Named (Scope () f a))
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | f a :>> f a
  -- | Sequencing with binding; analogous to '>>='.
  --
  --   Bindings made with :>>= are sequential, i.e. the name is not bound within the value, only within the consequence.
  | Named (f a) :>>= Scope () f a
  | Lam (Named (Scope () f a))
  -- | Function application; analogous to '$'.
  | f a :$ f a
  | Unit
  | Bool Bool
  | If (f a) (f a) (f a)
  | String Text
  -- | Load the specified file (by path).
  | Load (f a)
  -- | A record mapping some keys to some values.
  | Record [(Name, f a)]
  -- | Projection from a record.
  | f a :. Name
  -- | Projection of a record, with failure.
  | f a :? Name
  -- | Assignment of a value to the reference returned by the lhs.
  | f a := f a
  deriving (Foldable, Functor, Generic1, Traversable)

infixr 1 :>>
infixr 1 :>>=
infixl 8 :$
infixl 9 :.
infix  3 :=

instance HFunctor Core
instance HFoldable Core
instance HTraversable Core

instance RightModule Core where
  Rec b      >>=* f = Rec ((>>=* f) <$> b)
  (a :>> b)  >>=* f = (a >>= f) :>> (b >>= f)
  (a :>>= b) >>=* f = ((>>= f) <$> a) :>>= (b >>=* f)
  Lam b      >>=* f = Lam ((>>=* f) <$> b)
  (a :$ b)   >>=* f = (a >>= f) :$ (b >>= f)
  Unit       >>=* _ = Unit
  Bool b     >>=* _ = Bool b
  If c t e   >>=* f = If (c >>= f) (t >>= f) (e >>= f)
  String s   >>=* _ = String s
  Load b     >>=* f = Load (b >>= f)
  Record fs  >>=* f = Record (map (fmap (>>= f)) fs)
  (a :. b)   >>=* f = (a >>= f) :. b
  (a :? b)   >>=* f = (a >>= f) :. b
  (a := b)   >>=* f = (a >>= f) := (b >>= f)

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Core f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Core f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Core f a)


rec :: (Eq a, Has Core sig m) => Named a -> m a -> m a
rec (Named u n) b = send (Rec (Named u (abstract1 n b)))

(>>>) :: Has Core sig m => m a -> m a -> m a
a >>> b = send (a :>> b)

infixr 1 >>>

unseq :: (Alternative m, Project Core sig) => Term sig a -> m (Term sig a, Term sig a)
unseq = \case
  Alg sig | Just (a :>> b) <- prj sig -> pure (a, b)
  _                                   -> empty

unseqs :: Project Core sig => Term sig a -> NonEmpty (Term sig a)
unseqs = go
  where go t = case unseq t of
          Just (l, r) -> go l <> go r
          Nothing     -> t :| []

-- TODO: if the left hand side is only a unit, this should return just the RHS
-- this is a little fiddly to do
(>>>=) :: (Eq a, Has Core sig m) => (Named a :<- m a) -> m a -> m a
Named u n :<- a >>>= b = send (Named u a :>>= abstract1 n b)

infixr 1 >>>=

unbind :: (Alternative m, Project Core sig, RightModule sig) => a -> Term sig a -> m (Named a :<- Term sig a, Term sig a)
unbind n = \case
  Alg sig | Just (Named u a :>>= b) <- prj sig -> pure (Named u n :<- a, instantiate1 (pure n) b)
  _                                            -> empty

unstatement :: (Alternative m, Project Core sig, RightModule sig) => a -> Term sig a -> m (Maybe (Named a) :<- Term sig a, Term sig a)
unstatement n t = first (first Just) <$> unbind n t <|> first (Nothing :<-) <$> unseq t

do' :: (Eq a, Foldable t, Has Core sig m) => t (Maybe (Named a) :<- m a) -> m a
do' bindings = fromMaybe unit (foldr bind Nothing bindings)
  where bind (n :<- a) v = maybe (a >>>) ((>>>=) . (:<- a)) n <$> v <|> Just a

unstatements :: (Project Core sig, RightModule sig) => Term sig a -> (Stack (Maybe (Named (Either Int a)) :<- Term sig (Either Int a)), Term sig (Either Int a))
unstatements = unprefix (unstatement . Left) . fmap Right

data a :<- b = a :<- b
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infix 2 :<-

instance Bifunctor (:<-) where
  bimap f g (a :<- b) = f a :<- g b


lam :: (Eq a, Has Core sig m) => Named a -> m a -> m a
lam (Named u n) b = send (Lam (Named u (abstract1 n b)))

lams :: (Eq a, Foldable t, Has Core sig m) => t (Named a) -> m a -> m a
lams names body = foldr lam body names

unlam :: (Alternative m, Project Core sig, RightModule sig) => a -> Term sig a -> m (Named a, Term sig a)
unlam n = \case
  Alg sig | Just (Lam b) <- prj sig -> pure (n <$ b, instantiate1 (pure n) (namedValue b))
  _                                 -> empty

($$) :: Has Core sig m => m a -> m a -> m a
f $$ a = send (f :$ a)

infixl 8 $$

-- | Application of a function to a sequence of arguments.
($$*) :: (Foldable t, Has Core sig m) => m a -> t (m a) -> m a
($$*) = foldl' ($$)

infixl 8 $$*

unapply :: (Alternative m, Project Core sig) => Term sig a -> m (Term sig a, Term sig a)
unapply = \case
  Alg sig | Just (f :$ a) <- prj sig -> pure (f, a)
  _                                  -> empty

unapplies :: Project Core sig => Term sig a -> (Term sig a, Stack (Term sig a))
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

unit :: Has Core sig m => m a
unit = send Unit

bool :: Has Core sig m => Bool -> m a
bool = send . Bool

if' :: Has Core sig m => m a -> m a -> m a -> m a
if' c t e = send (If c t e)

string :: Has Core sig m => Text -> m a
string = send . String

load :: Has Core sig m => m a -> m a
load = send . Load

record :: Has Core sig m => [(Name, m a)] -> m a
record fs = send (Record fs)

(...) :: Has Core sig m => m a -> Name -> m a
a ... b = send (a :. b)

infixl 9 ...

(.?) :: Has Core sig m => m a -> Name -> m a
a .? b = send (a :? b)

infixl 9 .?

(.=) :: Has Core sig m => m a -> m a -> m a
a .= b = send (a := b)

infix 3 .=


data Ann ann f a
  = Ann ann (f a)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance HFunctor (Ann ann)
instance HFoldable (Ann ann)
instance HTraversable (Ann ann)

instance RightModule (Ann ann) where
  Ann l b >>=* f = Ann l (b >>= f)


ann :: Has (Ann Span) sig m => HasCallStack => m a -> m a
ann = annWith callStack

annAt :: Has (Ann ann) sig m => ann -> m a -> m a
annAt ann = send . Ann ann

annWith :: Has (Ann Span) sig m => CallStack -> m a -> m a
annWith callStack = maybe id (annAt . spanFromSrcLoc . snd) (listToMaybe (getCallStack callStack))


stripAnnotations :: forall ann a sig . RightModule sig => Term (Ann ann :+: sig) a -> Term sig a
stripAnnotations (Var v)             = Var v
stripAnnotations (Alg (L (Ann _ b))) = stripAnnotations b
stripAnnotations (Alg (R        b))  = Alg (hmap stripAnnotations b)
