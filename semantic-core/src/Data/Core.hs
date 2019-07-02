{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, QuantifiedConstraints, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Data.Core
( Core(..)
, CoreF(..)
, Edge(..)
, let'
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
, string
, load
, edge
, frame
, (...)
, (.=)
, ann
, annWith
, efold
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
  a >>= f = efold id let' (<>) (Core . Lam) ($$) unit bool if' string load edge frame (...) (.=) (fmap Core . Ann) pure f a


data CoreF f a
  = Let Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | f a :>> f a
  | Lam (Scope f a)
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

deriving instance (Eq   a, forall a . Eq   a => Eq   (f a), Monad f) => Eq   (CoreF f a)
deriving instance (Ord  a, forall a . Eq   a => Eq   (f a)
                         , forall a . Ord  a => Ord  (f a), Monad f) => Ord  (CoreF f a)
deriving instance (Show a, forall a . Show a => Show (f a))          => Show (CoreF f a)

infixl 2 :$
infixr 1 :>>
infix  3 :=
infixl 4 :.


let' :: Name -> Core a
let' = Core . Let

block :: Foldable t => t (Core a) -> Core a
block cs
  | null cs   = unit
  | otherwise = foldr1 (<>) cs

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

string :: Text -> Core a
string = Core . String

load :: Core a -> Core a
load = Core . Load

edge :: Edge -> Core a -> Core a
edge e b = Core (Edge e b)

frame :: Core a
frame = Core Frame

(...) :: Core a -> Core a -> Core a
a ... b = Core (a :. b)

(.=) :: Core a -> Core a -> Core a
a .= b = Core (a := b)

ann :: HasCallStack => Core a -> Core a
ann = annWith callStack

annWith :: CallStack -> Core a -> Core a
annWith callStack c = maybe c (flip (fmap Core . Ann) c) (stackLoc callStack)


efold :: forall m n a b
      .  (forall a . m a -> n a)
      -> (forall a . Name -> n a)
      -> (forall a . n a -> n a -> n a)
      -> (forall a . Scope n a -> n a)
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
      -> (a -> m b)
      -> Core a
      -> n b
efold var let' seq' lam app unit bool if' string load edge frame dot assign ann k = go
  where go :: forall x y . (x -> m y) -> Core x -> n y
        go h = \case
          Var a -> var (h a)
          Core c -> case c of
            Let a -> let' a
            a :>> b -> go h a `seq'` go h b
            Lam b -> lam (foldScope k go h b)
            a :$ b -> go h a `app` go h b
            Unit -> unit
            Bool b -> bool b
            If c t e -> if' (go h c) (go h t) (go h e)
            String s -> string s
            Load t -> load (go h t)
            Edge e t -> edge e (go h t)
            Frame -> frame
            a :. b -> go h a `dot` go h b
            a := b -> go h a `assign` go h b
            Ann loc t -> ann loc (go h t)

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
      -> (x -> a)
      -> Core x
      -> b
kfold var let' seq' lam app unit bool if' string load edge frame dot assign ann k h = getConst . efold (Const . var . getConst) (coerce let') (coerce seq') (coerce lam) (coerce app) (coerce unit) (coerce bool) (coerce if') (coerce string) (coerce load) (coerce edge) (coerce frame) (coerce dot) (coerce assign) (coerce ann) (coerce k) (Const . h)
