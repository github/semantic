{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, QuantifiedConstraints, RankNTypes,
             ScopedTypeVariables, StandaloneDeriving, TypeFamilies, TypeOperators #-}
module Data.Core
( Core(..)
, project
, embed
, CoreF(..)
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
, gfold
, efold
, eiter
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
  a >>= f = gfold id Let (:>>) Lam (:$) Unit Bool If String Load Edge Frame (:.) (:=) Ann pure (f <$> a)


project :: Core a -> Either a (CoreF Core a)
project (Var a)    = Left a
project (Let n)    = Right (LetF n)
project (a :>> b)  = Right (a :>>$ b)
project (Lam b)    = Right (LamF b)
project (f :$ a)   = Right (f :$$ a)
project Unit       = Right UnitF
project (Bool b)   = Right (BoolF b)
project (If c t e) = Right (IfF c t e)
project (String s) = Right (StringF s)
project (Load b)   = Right (LoadF b)
project (Edge e b) = Right (EdgeF e b)
project Frame      = Right FrameF
project (a :. b)   = Right (a :.$ b)
project (a := b)   = Right (a :=$ b)
project (Ann l b)  = Right (AnnF l b)

embed :: Either a (CoreF Core a) -> Core a
embed = either Var $ \case
  LetF n -> Let n
  a :>>$ b -> a :>> b
  LamF b -> Lam b
  f :$$ a -> f :$ a
  UnitF -> Unit
  BoolF b -> Bool b
  IfF c t e -> If c t e
  StringF s -> String s
  LoadF b -> Load b
  EdgeF e b -> Edge e b
  FrameF -> Frame
  a :.$ b -> a :. b
  a :=$ b -> a := b
  AnnF l b -> Ann l b


data CoreF f a
  = LetF Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | f a :>>$ f a
  | LamF (f (Incr (f a)))
  -- | Function application; analogous to '$'.
  | f a :$$ f a
  | UnitF
  | BoolF Bool
  | IfF (f a) (f a) (f a)
  | StringF Text
  -- | Load the specified file (by path).
  | LoadF (f a)
  | EdgeF Edge (f a)
  -- | Allocation of a new frame.
  | FrameF
  | f a :.$ f a
  -- | Assignment of a value to the reference returned by the lhs.
  | f a :=$ f a
  | AnnF Loc (f a)
  deriving (Foldable, Functor, Traversable)

deriving instance (Eq   a, forall x . Eq   x => Eq   (f x)) => Eq   (CoreF f a)
deriving instance (Ord  a, forall x . Eq   x => Eq   (f x)
                         , forall x . Ord  x => Ord  (f x)) => Ord  (CoreF f a)
deriving instance (Show a, forall x . Show x => Show (f x)) => Show (CoreF f a)

infixl 2 :$$
infixr 1 :>>$
infix  3 :=$
infixl 4 :.$


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
          Var a -> var a
          Let a -> let' a
          a :>> b -> go a `seq'` go b
          Lam b -> lam (go (k . fmap go <$> b))
          f :$ a -> go f `app` go a
          Unit -> unit
          Bool b -> bool b
          If c t e -> if' (go c) (go t) (go e)
          String s -> string s
          Load t -> load (go t)
          Edge e t -> edge e (go t)
          Frame -> frame
          a :. b -> go a `dot` go b
          a := b -> go a `assign` go b
          Ann loc t -> ann loc (go t)

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
efold var let' seq' lam app unit bool if' string load edge frame dot assign ann k = eiter var alg
  where alg :: forall x l' c z' . Functor c => (forall l'' z'' x . (l'' x -> m (z'' x)) -> c (l'' x) -> n (z'' x)) -> (l' x -> m (z' x)) -> CoreF c (l' x) -> n (z' x)
        alg go h = \case
          LetF a -> let' a
          a :>>$ b -> go h a `seq'` go h b
          LamF b -> lam (coerce (go
                      (coerce (k . fmap (go h))
                        :: ((Incr :.: c :.: l') x -> m ((Incr :.: n :.: z') x)))
                      (fmap coerce b))) -- FIXME: can we avoid this fmap and just coerce harder?
          a :$$ b -> go h a `app` go h b
          UnitF -> unit
          BoolF b -> bool b
          IfF c t e -> if' (go h c) (go h t) (go h e)
          StringF s -> string s
          LoadF t -> load (go h t)
          EdgeF e t -> edge e (go h t)
          FrameF -> frame
          a :.$ b -> go h a `dot` go h b
          a :=$ b -> go h a `assign` go h b
          AnnF loc t -> ann loc (go h t)

-- | Efficient Mendler-style iteration.
eiter :: forall l m n z b
      .  (forall a . m a -> n a)
      -> (forall a l' c z' . Functor c => (forall l'' z'' x . (l'' x -> m (z'' x)) -> c (l'' x) -> n (z'' x)) -> (l' a -> m (z' a)) -> CoreF c (l' a) -> n (z' a))
      -> (l b -> m (z b))
      -> Core (l b)
      -> n (z b)
eiter var alg = go
  where go :: forall l' z' x . (l' x -> m (z' x)) -> Core (l' x) -> n (z' x)
        go h c = case project c of
          Left a  -> var (h a)
          Right b -> alg go h b

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
