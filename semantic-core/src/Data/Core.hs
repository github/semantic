{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, RankNTypes, ScopedTypeVariables,
             TypeFamilies #-}
module Data.Core
( Core(..)
, Edge(..)
, lams
, ($$*)
, unapply
, unapplies
, block
, ann
, annWith
) where

import Control.Applicative (Alternative (..))
import Control.Monad (ap)
import Data.Foldable (foldl')
import Data.Loc
import Data.Name
import Data.Stack
import Data.Text (Text)
import GHC.Stack

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

data Core a
  = Var a
  | Let Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | Core a :>> Core a
  | Lam (Core (Incr a))
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
  a >>= f = gfoldT id Let (:>>) Lam (:$) Unit Bool If String Load Edge Frame (:.) (:=) Ann (incr (pure Z) (fmap S)) (f <$> a)


lam :: Eq a => a -> Core a -> Core a
lam n b = Lam (bind n b)

lams :: (Eq a, Foldable t) => t a -> Core a -> Core a
lams names body = foldr lam body names

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

block :: Foldable t => t (Core a) -> Core a
block cs
  | null cs   = Unit
  | otherwise = foldr1 (:>>) cs

ann :: HasCallStack => Core a -> Core a
ann = annWith callStack

annWith :: CallStack -> Core a -> Core a
annWith callStack c = maybe c (flip Ann c) (stackLoc callStack)


gfoldT :: forall m n b
       .  (forall a . m a -> n a)
       -> (forall a . Name -> n a)
       -> (forall a . n a -> n a -> n a)
       -> (forall a . n (Incr a) -> n a)
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
       -> (forall a . Incr (m a) -> m (Incr a))
       -> Core (m b)
       -> n b
gfoldT var let' seq' lam app unit bool if' string load edge frame dot assign ann dist = go
  where go :: Core (m x) -> n x
        go = \case
          Var a -> var a
          Let a -> let' a
          a :>> b -> go a `seq'` go b
          Lam b -> lam (go (dist <$> b))
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


-- | Bind occurrences of a name in a 'Value' term, producing a 'Core' in which the name is bound.
bind :: Eq a => a -> Core a -> Core (Incr a)
bind name = fmap (match name)
