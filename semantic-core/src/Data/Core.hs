{-# LANGUAGE DeriveTraversable, FlexibleContexts, LambdaCase, OverloadedStrings, RecordWildCards, TemplateHaskell,
             TypeFamilies #-}
module Data.Core
( Core(..)
, CoreF(..)
, Edge(..)
, lams
, ($$*)
, unapply
, unapplies
, block
, ann
, annWith
, stripAnnotations
) where

import Control.Applicative (Alternative (..))
import Data.Foldable (foldl')
import Data.Functor.Foldable hiding (ListF (..))
import Data.Functor.Foldable.TH
import Data.Loc
import Data.Name
import Data.Stack
import Data.Text (Text)
import GHC.Stack

data Edge = Lexical | Import
  deriving (Eq, Ord, Show)

data Core
  = Var Name
  | Let Name
  -- | Sequencing without binding; analogous to '>>' or '*>'.
  | Core :>> Core
  | Lam Name Core
  -- | Function application; analogous to '$'.
  | Core :$ Core
  | Unit
  | Bool Bool
  | If Core Core Core
  | String Text
  -- | Load the specified file (by path).
  | Load Core
  | Edge Edge Core
  -- | Allocation of a new frame.
  | Frame
  | Core :. Core
  -- | Assignment of a value to the reference returned by the lhs.
  | Core := Core
  | Ann Loc Core
  deriving (Eq, Ord, Show)

infixl 2 :$
infixr 1 :>>
infix  3 :=
infixl 4 :.

makeBaseFunctor ''Core

infixl 2 :$$
infixr 1 :>>$
infix  3 :=$
infixl 4 :.$

instance Semigroup Core where
  (<>) = (:>>)

lams :: Foldable t => t Name -> Core -> Core
lams names body = foldr Lam body names

-- | Application of a function to a sequence of arguments.
($$*) :: Foldable t => Core -> t Core -> Core
($$*) = foldl' (:$)

infixl 9 $$*

unapply :: Alternative m => Core -> m (Core, Core)
unapply (f :$ a) = pure (f, a)
unapply _        = empty

unapplies :: Core -> (Core, Stack Core)
unapplies core = case unapply core of
  Just (f, a) -> (:> a) <$> unapplies f
  Nothing     -> (core, Nil)

block :: Foldable t => t Core -> Core
block cs
  | null cs   = Unit
  | otherwise = foldr1 (:>>) cs

ann :: HasCallStack => Core -> Core
ann = annWith callStack

annWith :: CallStack -> Core -> Core
annWith callStack c = maybe c (flip Ann c) (stackLoc callStack)

stripAnnotations :: Core -> Core
stripAnnotations = cata go where
  go (AnnF _ item) = item
  go item          = embed item
