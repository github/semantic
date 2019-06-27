{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, QuantifiedConstraints, RankNTypes, ScopedTypeVariables, StandaloneDeriving,
             TypeFamilies #-}
module Data.Core
( Core(..)
, CoreF(..)
, Edge(..)
, let'
, (>>>)
, block
, lam
, lams
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
, load
, edge
, frame
, (...)
, (.=)
, ann
, annWith
, gfold
, kfold
, instantiate
) where

import Control.Applicative (Alternative (..), Const (..))
import Control.Monad (ap)
import Data.Coerce
import Data.Foldable (foldl')
import Data.List.NonEmpty
import Data.Loc
import Data.Name
import Data.Stack
import Data.Text (Text)
import GHC.Stack

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

newtype Core a = Core { unCore :: CoreF Core a }
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

data CoreF f a
  = Var a
  | Let Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | f a :>> f a
  | Lam (f (Incr (f a)))
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
  deriving (Foldable, Functor, Traversable)

deriving instance (Eq   a, forall x . Eq   x => Eq   (f x)) => Eq   (CoreF f a)
deriving instance (Ord  a, forall x . Eq   x => Eq   (f x)
                         , forall x . Ord  x => Ord  (f x)) => Ord  (CoreF f a)
deriving instance (Show a, forall x . Show x => Show (f x)) => Show (CoreF f a)

infixl 2 :$
infixr 1 :>>
infix  3 :=
infixl 4 :.

instance Semigroup (Core a) where
  (<>) = fmap Core . (:>>)

instance Applicative Core where
  pure = Core . Var
  (<*>) = ap

instance Monad Core where
  a >>= f = gfold id (Core . Let) (fmap Core . (:>>)) (Core . Lam) (fmap Core . (:$)) (Core Unit) (Core . Bool) (\ c t e -> Core (If c t e)) (Core . String) (Core . Load) (fmap Core . Edge) (Core Frame) (fmap Core . (:.)) (fmap Core . (:=)) (fmap Core . Ann) pure (f <$> a)


let' :: Name -> Core a
let' = Core . Let

(>>>) :: Core a -> Core a -> Core a
a >>> b = Core (a :>> b)

infixr 1 >>>

block :: Foldable t => t (Core a) -> Core a
block cs
  | null cs   = unit
  | otherwise = foldr1 (>>>) cs

lam :: Eq a => a -> Core a -> Core a
lam n b = Core (Lam (bind n b))

lams :: (Eq a, Foldable t) => t a -> Core a -> Core a
lams names body = foldr lam body names

unlam :: Alternative m => a -> Core a -> m (a, Core a)
unlam n (Core (Lam b)) = pure (n, instantiate (pure n) b)
unlam _ _              = empty

unseq :: Alternative m => Core a -> m (Core a, Core a)
unseq (Core (a :>> b)) = pure (a, b)
unseq _                = empty

unseqs :: Core a -> NonEmpty (Core a)
unseqs = go
  where go t = case unseq t of
          Just (l, r) -> go l <> go r
          Nothing     -> t :| []

($$) :: Core a -> Core a -> Core a
f $$ a = Core (f :$ a)

infixl 2 $$

-- | Application of a function to a sequence of arguments.
($$*) :: Foldable t => Core a -> t (Core a) -> Core a
($$*) = foldl' ($$)

infixl 9 $$*

unapply :: Alternative m => Core a -> m (Core a, Core a)
unapply (Core (f :$ a)) = pure (f, a)
unapply _               = empty

unapplies :: Core a -> (Core a, Stack (Core a))
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

unit :: Core a
unit = Core Unit

bool :: Bool -> Core a
bool = Core . Bool

if' :: Core a -> Core a -> Core a -> Core a
if' c t e = Core (If c t e)

load :: Core a -> Core a
load = Core . Load

edge :: Edge -> Core a -> Core a
edge e b = Core (Edge e b)

frame :: Core a
frame = Core Frame

(...) :: Core a -> Core a -> Core a
a ... b = Core (a :. b)

infixl 4 ...

(.=) :: Core a -> Core a -> Core a
a .= b = Core (a := b)

infix 3 .=

ann :: HasCallStack => Core a -> Core a
ann = annWith callStack

annWith :: CallStack -> Core a -> Core a
annWith callStack c = maybe c (flip (fmap Core . Ann) c) (stackLoc callStack)


gfold :: forall m n b
      .  (forall a . m a -> n a)
      -> (forall a . Name -> n a)
      -> (forall a . n a -> n a -> n a)
      -> (forall a . n (Incr (n a)) -> n a)
      -> (forall a . n a -> n a -> n a)
      -> (forall a . n a)
      -> (forall a . Bool -> n a)
      -> (forall a . n a -> n a -> n a -> n a)
      -> (forall a . Text -> n a)
      -> (forall a . n a -> n a)
      -> (forall a . Edge -> n a -> n a)
      -> (forall a . n a)
      -> (forall a . n a -> n a -> n a)
      -> (forall a . n a -> n a -> n a)
      -> (forall a . Loc -> n a -> n a)
      -> (forall a . Incr (n a) -> m (Incr (n a)))
      -> Core (m b)
      -> n b
gfold var let' seq' lam app unit bool if' string load edge frame dot assign ann k = go
  where go :: Core (m x) -> n x
        go = \case
          Core (Var a) -> var a
          Core (Let a) -> let' a
          Core (a :>> b) -> go a `seq'` go b
          Core (Lam b) -> lam (go (k . fmap go <$> b))
          Core (f :$ a) -> go f `app` go a
          Core Unit -> unit
          Core (Bool b) -> bool b
          Core (If c t e) -> if' (go c) (go t) (go e)
          Core (String s) -> string s
          Core (Load t) -> load (go t)
          Core (Edge e t) -> edge e (go t)
          Core Frame -> frame
          Core (a :. b) -> go a `dot` go b
          Core (a := b) -> go a `assign` go b
          Core (Ann loc t) -> ann loc (go t)

kfold :: (a -> b)
      -> (Name -> b)
      -> (b -> b -> b)
      -> (b -> b)
      -> (b -> b -> b)
      -> b
      -> (Bool -> b)
      -> (b -> b -> b -> b)
      -> (Text -> b)
      -> (b -> b)
      -> (Edge -> b -> b)
      -> b
      -> (b -> b -> b)
      -> (b -> b -> b)
      -> (Loc -> b -> b)
      -> (Incr b -> a)
      -> Core a
      -> b
kfold var let' seq' lam app unit bool if' string load edge frame dot assign ann k = getConst . gfold (coerce var) (coerce let') (coerce seq') (coerce lam) (coerce app) (coerce unit) (coerce bool) (coerce if') (coerce string) (coerce load) (coerce edge) (coerce frame) (coerce dot) (coerce assign) (coerce ann) (coerce k) . fmap Const


-- | Bind occurrences of a name in a 'Core' term, producing a 'Core' in which the name is bound.
bind :: Eq a => a -> Core a -> Core (Incr (Core a))
bind name = fmap (fmap pure . match name)

-- | Substitute a 'Core' term for the free variable in a given 'Core', producing a closed 'Core' term.
instantiate :: Core a -> Core (Incr (Core a)) -> Core a
instantiate t b = b >>= subst t
