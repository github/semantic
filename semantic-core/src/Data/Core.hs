{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, QuantifiedConstraints, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Data.Core
( Core(..)
, Edge(..)
, block
, lam
, lams
, unlam
, unseq
, unseqs
, ($$*)
, unapply
, unapplies
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
import Data.Functor.Identity
import Data.List.NonEmpty
import Data.Loc
import Data.Name
import Data.Stack
import Data.Text (Text)
import GHC.Generics ((:.:) (..))
import GHC.Stack

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

data Core a
  = Var a
  | Let Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | Core a :>> Core a
  | Lam (Core (Incr (Core a)))
  -- | Function application; analogous to '$'.
  | Core a :$ Core a
  | Unit
  | Bool Bool
  | If (Core a) (Core a) (Core a)
  | String Text
  -- | Load the specified file (by path).
  | Load (Core a)
  | Edge Edge (Core a)
  -- | Allocation of a new frame.
  | Frame
  | Core a :. Core a
  -- | Assignment of a value to the reference returned by the lhs.
  | Core a := Core a
  | Ann Loc (Core a)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 2 :$
infixr 1 :>>
infix  3 :=
infixl 4 :.

instance Semigroup (Core a) where
  (<>) = (:>>)

instance Applicative Core where
  pure = Var
  (<*>) = ap

instance Monad Core where
  a >>= f = coerce $ efold id Let (:>>) Lam (:$) Unit Bool If String Load Edge Frame (:.) (:=) Ann pure ((coerce `asTypeOf` fmap Const) . f . runIdentity) (coerce a)


block :: Foldable t => t (Core a) -> Core a
block cs
  | null cs   = Unit
  | otherwise = foldr1 (<>) cs

lam :: Eq a => a -> Core a -> Core a
lam n b = Lam (bind n b)

lams :: (Eq a, Foldable t) => t a -> Core a -> Core a
lams names body = foldr lam body names

unlam :: Alternative m => a -> Core a -> m (a, Core a)
unlam n (Lam b) = pure (n, instantiate (pure n) b)
unlam _ _       = empty

unseq :: Alternative m => Core a -> m (Core a, Core a)
unseq (a :>> b) = pure (a, b)
unseq _         = empty

unseqs :: Core a -> NonEmpty (Core a)
unseqs = go
  where go t = case unseq t of
          Just (l, r) -> go l <> go r
          Nothing     -> t :| []

-- | Application of a function to a sequence of arguments.
($$*) :: Foldable t => Core a -> t (Core a) -> Core a
($$*) = foldl' (:$)

infixl 9 $$*

unapply :: Alternative m => Core a -> m (Core a, Core a)
unapply (f :$ a) = pure (f, a)
unapply _        = empty

unapplies :: Core a -> (Core a, Stack (Core a))
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

ann :: HasCallStack => Core a -> Core a
ann = annWith callStack

annWith :: CallStack -> Core a -> Core a
annWith callStack c = maybe c (flip Ann c) (stackLoc callStack)


efold :: forall l m n z b
      .  ( forall a b . Coercible a b => Coercible (n a) (n b)
         , forall a b . Coercible a b => Coercible (m a) (m b)
         )
      => (forall a . m a -> n a)
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
      -> (l b -> m (z b))
      -> Core (l b)
      -> n (z b)
efold var let' seq' lam app unit bool if' string load edge frame dot assign ann k = go
  where go :: forall x l' z' . (l' x -> m (z' x)) -> Core (l' x) -> n (z' x)
        go h = \case
          Var a -> var (h a)
          Let a -> let' a
          a :>> b -> go h a `seq'` go h b
          Lam b -> lam (coerce (go
                     (coerce (k . fmap (go h))
                       :: (Incr :.: Core :.: l') x -> m ((Incr :.: n :.: z') x))
                     (coerce b)))
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
      -> Core a
      -> b
kfold var let' seq' lam app unit bool if' string load edge frame dot assign ann k = getConst . efold (Const . var . getConst) (coerce let') (coerce seq') (coerce lam) (coerce app) (coerce unit) (coerce bool) (coerce if') (coerce string) (coerce load) (coerce edge) (coerce frame) (coerce dot) (coerce assign) (coerce ann) (coerce k) coerce . (coerce `asTypeOf` fmap Const)
