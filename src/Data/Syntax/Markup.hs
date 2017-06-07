{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Markup where

import Data.Align.Generic
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics
import Prologue hiding (Text)

newtype Document a = Document [a]
 deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Document where liftEq = genericLiftEq
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec

newtype Paragraph a = Paragraph [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec

data Heading a = Heading { headingLevel :: Int, headingContent :: [a] }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Heading where liftEq = genericLiftEq
instance Show1 Heading where liftShowsPrec = genericLiftShowsPrec

newtype Strong a = Strong [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Strong where liftEq = genericLiftEq
instance Show1 Strong where liftShowsPrec = genericLiftShowsPrec

newtype Emphasis a = Emphasis [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Emphasis where liftEq = genericLiftEq
instance Show1 Emphasis where liftShowsPrec = genericLiftShowsPrec

newtype Text a = Text ByteString
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec
