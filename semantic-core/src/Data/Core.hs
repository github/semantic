{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, OverloadedStrings, QuantifiedConstraints, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Core
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
, (.=)
, Ann(..)
, ann
, annAt
, annWith
, instantiate
, stripAnnotations
) where

import Control.Applicative (Alternative (..))
import Control.Carrier
import Control.Monad.Module
import Data.Bifunctor (Bifunctor (..))
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Loc
import Data.Maybe (fromMaybe)
import Data.Name
import Data.Scope
import Data.Stack
import Data.Term
import Data.Text (Text)
import GHC.Generics (Generic1)
import GHC.Stack

data Core f a
  -- | Recursive local binding of a name in a scope; strict evaluation of the name in the body will diverge.
  --
  --   Simultaneous (and therefore potentially mutually-recursive) bidnings can be made by binding a 'Record' recursively within 'Rec' and projecting from it with ':.'.
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
  -- | Assignment of a value to the reference returned by the lhs.
  | f a := f a
  deriving (Foldable, Functor, Generic1, Traversable)

infixr 1 :>>
infixr 1 :>>=
infixl 8 :$
infixl 9 :.
infix  3 :=

instance HFunctor Core

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Core f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Core f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Core f a)

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
  (a := b)   >>=* f = (a >>= f) := (b >>= f)


rec :: (Eq a, Has Core sig m) => Named a -> m a -> m a
rec (Named u n) b = send (Rec (Named u (abstract1 n b)))

(>>>) :: Has Core sig m => m a -> m a -> m a
a >>> b = send (a :>> b)

infixr 1 >>>

unseq :: (Alternative m, Project Core sig) => Term sig a -> m (Term sig a, Term sig a)
unseq (Term sig) | Just (a :>> b) <- prj sig = pure (a, b)
unseq _                                      = empty

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
unbind n (Term sig) | Just (Named u a :>>= b) <- prj sig = pure (Named u n :<- a, instantiate1 (pure n) b)
unbind _ _                                               = empty

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
unlam n (Term sig) | Just (Lam b) <- prj sig = pure (n <$ b, instantiate1 (pure n) (namedValue b))
unlam _ _                                    = empty

($$) :: Has Core sig m => m a -> m a -> m a
f $$ a = send (f :$ a)

infixl 8 $$

-- | Application of a function to a sequence of arguments.
($$*) :: (Foldable t, Has Core sig m) => m a -> t (m a) -> m a
($$*) = foldl' ($$)

infixl 8 $$*

unapply :: (Alternative m, Project Core sig) => Term sig a -> m (Term sig a, Term sig a)
unapply (Term sig) | Just (f :$ a) <- prj sig = pure (f, a)
unapply _                                     = empty

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

(.=) :: Has Core sig m => m a -> m a -> m a
a .= b = send (a := b)

infix 3 .=


data Ann f a
  = Ann Loc (f a)
  deriving (Eq, Foldable, Functor, Generic1, Ord, Show, Traversable)

instance HFunctor Ann

instance RightModule Ann where
  Ann l b >>=* f = Ann l (b >>= f)


ann :: Has Ann sig m => HasCallStack => m a -> m a
ann = annWith callStack

annAt :: Has Ann sig m => Loc -> m a -> m a
annAt loc = send . Ann loc

annWith :: Has Ann sig m => CallStack -> m a -> m a
annWith callStack = maybe id annAt (stackLoc callStack)


stripAnnotations :: (HFunctor sig, forall g . Functor g => Functor (sig g)) => Term (Ann :+: sig) a -> Term sig a
stripAnnotations (Var v)              = Var v
stripAnnotations (Term (L (Ann _ b))) = stripAnnotations b
stripAnnotations (Term (R        b))  = Term (hmap stripAnnotations b)


-- | The class of types which can be projected from a signature.
--
--   This is based on Wouter Swierstra’s design described in [Data types à la carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf). As described therein, overlapping instances are required in order to distinguish e.g. left-occurrence from right-recursion.
--
--   It should not generally be necessary for you to define new 'Project' instances, but these are not specifically prohibited if you wish to get creative.
class Project (sub :: (* -> *) -> (* -> *)) sup where
  -- | Inject a member of a signature into the signature.
  prj :: sup m a -> Maybe (sub m a)

-- | Reflexivity: @t@ is a member of itself.
instance Project t t where
  prj = Just

-- | Left-recursion: if @t@ is a member of @l1 ':+:' l2 ':+:' r@, then we can inject it into @(l1 ':+:' l2) ':+:' r@ by injection into a right-recursive signature, followed by left-association.
instance {-# OVERLAPPABLE #-}
         Project t (l1 :+: l2 :+: r)
      => Project t ((l1 :+: l2) :+: r) where
  prj = prj . reassoc where
    reassoc (L (L l)) = L l
    reassoc (L (R l)) = R (L l)
    reassoc (R r)     = R (R r)

-- | Left-occurrence: if @t@ is at the head of a signature, we can inject it in O(1).
instance {-# OVERLAPPABLE #-}
         Project l (l :+: r) where
  prj (L l) = Just l
  prj _     = Nothing

-- | Right-recursion: if @t@ is a member of @r@, we can inject it into @r@ in O(n), followed by lifting that into @l ':+:' r@ in O(1).
instance {-# OVERLAPPABLE #-}
         Project l r
      => Project l (l' :+: r) where
  prj (R r) = prj r
  prj _     = Nothing
