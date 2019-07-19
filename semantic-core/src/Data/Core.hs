{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, QuantifiedConstraints, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Data.Core
( Core(..)
, Edge(..)
, let'
, block
, lam
, lam'
, lams
, lams'
, unlam
, unseq
, unseqs
, ($$)
, ($$*)
, unapply
, unapplies
, unit
, bool
, if'
, string
, load
, edge
, frame
, record
, (...)
, (.=)
, ann
, annWith
, instantiate
, stripAnnotations
) where

import Control.Applicative (Alternative (..))
import Control.Effect.Carrier
import Control.Monad.Module
import Data.Foldable (foldl')
import Data.List (elemIndex)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Loc
import Data.Name
import Data.Scope
import Data.Stack
import Data.Term
import Data.Text (Text)
import GHC.Generics (Generic1)
import GHC.Stack

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

data Core f a
  = Let User
  | Rec (Named (Scope () f a))
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | f a :>> f a
  | Lam (Named (Scope () f a))
  -- | Function application; analogous to '$'.
  | f a :$ f a
  | Unit
  | Bool Bool
  | If (f a) (f a) (f a)
  | String Text
  -- | Load the specified file (by path).
  | Load (f a)
  | Edge Edge (f a)
  -- | Allocation of a new frame.
  | Frame
  -- | A record holding simultaneously-bound, potentially mutually-recursive definitions.
  | Record [Named (Scope Int f a)]
  | f a :. f a
  -- | Assignment of a value to the reference returned by the lhs.
  | f a := f a
  | Ann Loc (f a)
  deriving (Foldable, Functor, Generic1, Traversable)

infixr 1 :>>
infixl 9 :$
infixl 4 :.
infix  3 :=

instance HFunctor Core

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (Core f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (Core f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (Core f a)

instance RightModule Core where
  Let u     >>=* _ = Let u
  Rec b     >>=* f = Rec ((>>=* f) <$> b)
  (a :>> b) >>=* f = (a >>= f) :>> (b >>= f)
  Lam b     >>=* f = Lam ((>>=* f) <$> b)
  (a :$ b)  >>=* f = (a >>= f) :$ (b >>= f)
  Unit      >>=* _ = Unit
  Bool b    >>=* _ = Bool b
  If c t e  >>=* f = If (c >>= f) (t >>= f) (e >>= f)
  String s  >>=* _ = String s
  Load b    >>=* f = Load (b >>= f)
  Edge e b  >>=* f = Edge e (b >>= f)
  Frame     >>=* _ = Frame
  Record fs >>=* f = Record (map (fmap (>>=* f)) fs)
  (a :. b)  >>=* f = (a >>= f) :. (b >>= f)
  (a := b)  >>=* f = (a >>= f) := (b >>= f)
  Ann l b   >>=* f = Ann l (b >>= f)


let' :: (Carrier sig m, Member Core sig) => User -> m a
let' = send . Let

block :: (Foldable t, Carrier sig m, Member Core sig) => t (m a) -> m a
block = maybe unit getBlock . foldMap (Just . Block)

newtype Block m a = Block { getBlock :: m a }

instance (Carrier sig m, Member Core sig) => Semigroup (Block m a) where
  Block a <> Block b = Block (send (a :>> b))

lam :: (Eq a, Carrier sig m, Member Core sig) => Named a -> m a -> m a
lam (Named u n) b = send (Lam (Named u (bind1 n b)))

lam' :: (Carrier sig m, Member Core sig) => User -> m User -> m User
lam' u = lam (named' u)

lams :: (Eq a, Foldable t, Carrier sig m, Member Core sig) => t (Named a) -> m a -> m a
lams names body = foldr lam body names

lams' :: (Foldable t, Carrier sig m, Member Core sig) => t User -> m User -> m User
lams' names body = foldr lam' body names

unlam :: (Alternative m, Member Core sig, RightModule sig) => a -> Term sig a -> m (Named a, Term sig a)
unlam n (Term sig) | Just (Lam b) <- prj sig = pure (n <$ b, instantiate1 (pure n) (namedValue b))
unlam _ _                                    = empty

unseq :: (Alternative m, Member Core sig) => Term sig a -> m (Term sig a, Term sig a)
unseq (Term sig) | Just (a :>> b) <- prj sig = pure (a, b)
unseq _                                      = empty

unseqs :: Member Core sig => Term sig a -> NonEmpty (Term sig a)
unseqs = go
  where go t = case unseq t of
          Just (l, r) -> go l <> go r
          Nothing     -> t :| []

($$) :: (Carrier sig m, Member Core sig) => m a -> m a -> m a
f $$ a = send (f :$ a)

infixl 9 $$

-- | Application of a function to a sequence of arguments.
($$*) :: (Foldable t, Carrier sig m, Member Core sig) => m a -> t (m a) -> m a
($$*) = foldl' ($$)

infixl 9 $$*

unapply :: (Alternative m, Member Core sig) => Term sig a -> m (Term sig a, Term sig a)
unapply (Term sig) | Just (f :$ a) <- prj sig = pure (f, a)
unapply _                                     = empty

unapplies :: Member Core sig => Term sig a -> (Term sig a, Stack (Term sig a))
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

unit :: (Carrier sig m, Member Core sig) => m a
unit = send Unit

bool :: (Carrier sig m, Member Core sig) => Bool -> m a
bool = send . Bool

if' :: (Carrier sig m, Member Core sig) => m a -> m a -> m a -> m a
if' c t e = send (If c t e)

string :: (Carrier sig m, Member Core sig) => Text -> m a
string = send . String

load :: (Carrier sig m, Member Core sig) => m a -> m a
load = send . Load

edge :: (Carrier sig m, Member Core sig) => Edge -> m a -> m a
edge e b = send (Edge e b)

frame :: (Carrier sig m, Member Core sig) => m a
frame = send Frame

record :: (Eq a, Carrier sig m, Member Core sig) => [(Named a, m a)] -> m a
record fs = send (Record (map bind' fs))
  where bind' (n, f) = bind (`elemIndex` map (namedValue . fst) fs) f <$ n

(...) :: (Carrier sig m, Member Core sig) => m a -> m a -> m a
a ... b = send (a :. b)

infixl 4 ...

(.=) :: (Carrier sig m, Member Core sig) => m a -> m a -> m a
a .= b = send (a := b)

infix 3 .=

ann :: (Carrier sig m, Member Core sig) => HasCallStack => m a -> m a
ann = annWith callStack

annWith :: (Carrier sig m, Member Core sig) => CallStack -> m a -> m a
annWith callStack = maybe id (fmap send . Ann) (stackLoc callStack)


stripAnnotations :: (Member Core sig, HFunctor sig, forall g . Functor g => Functor (sig g)) => Term sig a -> Term sig a
stripAnnotations (Var v)  = Var v
stripAnnotations (Term t)
  | Just c <- prj t, Ann _ b <- c = stripAnnotations b
  | otherwise                     = Term (hmap stripAnnotations t)
