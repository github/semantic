{-# LANGUAGE DeriveGeneric, DeriveTraversable, FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, QuantifiedConstraints, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Data.Core
( Core(..)
, CoreF(..)
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
, (...)
, (.=)
, ann
, annWith
, iter
, cata
, instantiate
) where

import Control.Applicative (Alternative (..), Const (..))
import Control.Effect.Carrier
import Control.Monad (ap)
import Data.Coerce
import Data.Foldable (foldl')
import Data.List.NonEmpty
import Data.Loc
import Data.Maybe
import Data.Name
import Data.Scope
import Data.Stack
import Data.Text (Text)
import GHC.Generics (Generic1)
import GHC.Stack

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

data Core a
  = Var a
  | Core (CoreF Core a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

instance Semigroup (Core a) where
  a <> b = Core (a :>> b)

instance Applicative Core where
  pure = Var
  (<*>) = ap

instance Monad Core where
  a >>= f = iter id Core Var f a

instance Carrier CoreF Core where
  eff = Core


data CoreF f a
  = Let User
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | f a :>> f a
  | Lam (Ignored User) (Scope () f a)
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
  | f a :. f a
  -- | Assignment of a value to the reference returned by the lhs.
  | f a := f a
  | Ann Loc (f a)
  deriving (Foldable, Functor, Generic1, Traversable)

instance HFunctor CoreF

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (CoreF f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (CoreF f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (CoreF f a)

infixl 2 :$
infixr 1 :>>
infix  3 :=
infixl 4 :.


let' :: (Carrier sig m, Member CoreF sig) => User -> m a
let' = send . Let

block :: Foldable t => t (Core a) -> Core a
block = fromMaybe unit . foldMap Just

lam :: (Eq a, Carrier sig m, Member CoreF sig) => Named a -> m a -> m a
lam (Named u n) b = send (Lam u (bind1 n b))

lam' :: (Carrier sig m, Member CoreF sig) => User -> m User -> m User
lam' u = lam (named u u)

lams :: (Eq a, Foldable t, Carrier sig m, Member CoreF sig) => t (Named a) -> m a -> m a
lams names body = foldr lam body names

lams' :: (Foldable t, Carrier sig m, Member CoreF sig) => t User -> m User -> m User
lams' names body = foldr lam' body names

unlam :: Alternative m => a -> Core a -> m (Named a, Core a)
unlam n (Core (Lam v b)) = pure (Named v n, instantiate (const (pure n)) b)
unlam _ _                = empty

unseq :: Alternative m => Core a -> m (Core a, Core a)
unseq (Core (a :>> b)) = pure (a, b)
unseq _                = empty

unseqs :: Core a -> NonEmpty (Core a)
unseqs = go
  where go t = case unseq t of
          Just (l, r) -> go l <> go r
          Nothing     -> t :| []

($$) :: (Carrier sig m, Member CoreF sig) => m a -> m a -> m a
f $$ a = send (f :$ a)

infixl 2 $$

-- | Application of a function to a sequence of arguments.
($$*) :: (Foldable t, Carrier sig m, Member CoreF sig) => m a -> t (m a) -> m a
($$*) = foldl' ($$)

infixl 9 $$*

unapply :: Alternative m => Core a -> m (Core a, Core a)
unapply (Core (f :$ a)) = pure (f, a)
unapply _               = empty

unapplies :: Core a -> (Core a, Stack (Core a))
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

unit :: (Carrier sig m, Member CoreF sig) => m a
unit = send Unit

bool :: (Carrier sig m, Member CoreF sig) => Bool -> m a
bool = send . Bool

if' :: (Carrier sig m, Member CoreF sig) => m a -> m a -> m a -> m a
if' c t e = send (If c t e)

string :: (Carrier sig m, Member CoreF sig) => Text -> m a
string = send . String

load :: (Carrier sig m, Member CoreF sig) => m a -> m a
load = send . Load

edge :: (Carrier sig m, Member CoreF sig) => Edge -> m a -> m a
edge e b = send (Edge e b)

frame :: (Carrier sig m, Member CoreF sig) => m a
frame = send Frame

(...) :: (Carrier sig m, Member CoreF sig) => m a -> m a -> m a
a ... b = send (a :. b)

(.=) :: (Carrier sig m, Member CoreF sig) => m a -> m a -> m a
a .= b = send (a := b)

ann :: (Carrier sig m, Member CoreF sig) => HasCallStack => m a -> m a
ann = annWith callStack

annWith :: (Carrier sig m, Member CoreF sig) => CallStack -> m a -> m a
annWith callStack = maybe id (fmap send . Ann) (stackLoc callStack)


iter :: forall m n a b
     .  (forall a . m a -> n a)
     -> (forall a . CoreF n a -> n a)
     -> (forall a . Incr () (n a) -> m (Incr () (n a)))
     -> (a -> m b)
     -> Core a
     -> n b
iter var alg k = go
  where go :: forall x y . (x -> m y) -> Core x -> n y
        go h = \case
          Var a -> var (h a)
          Core c -> alg (foldCoreF k go h c)

cata :: (a -> b)
     -> (forall a . CoreF (Const b) a -> b)
     -> (Incr () b -> a)
     -> (x -> a)
     -> Core x
     -> b
cata var alg k h = getConst . iter (coerce var) (coerce alg) (coerce k) (Const . h)

foldCoreF :: (forall a . Incr () (n a) -> m (Incr () (n a)))
          -> (forall x y . (x -> m y) -> f x -> n y)
          -> (a -> m b)
          -> CoreF f a
          -> CoreF n b
foldCoreF k go h = \case
  Let a -> Let a
  a :>> b -> go h a :>> go h b
  Lam u b -> Lam u (foldScope k go h b)
  a :$ b -> go h a :$ go h b
  Unit -> Unit
  Bool b -> Bool b
  If c t e -> If (go h c) (go h t) (go h e)
  String s -> String s
  Load t -> Load (go h t)
  Edge e t -> Edge e (go h t)
  Frame -> Frame
  a :. b -> go h a :. go h b
  a := b -> go h a := go h b
  Ann loc t -> Ann loc (go h t)
