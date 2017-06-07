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


-- Block elements

newtype Paragraph a = Paragraph [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec

data Heading a = Heading { headingLevel :: Int, headingContent :: [a] }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Heading where liftEq = genericLiftEq
instance Show1 Heading where liftShowsPrec = genericLiftShowsPrec

newtype UnorderedList a = UnorderedList [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 UnorderedList where liftEq = genericLiftEq
instance Show1 UnorderedList where liftShowsPrec = genericLiftShowsPrec

newtype OrderedList a = OrderedList [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 OrderedList where liftEq = genericLiftEq
instance Show1 OrderedList where liftShowsPrec = genericLiftShowsPrec

newtype BlockQuote a = BlockQuote [a]
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 BlockQuote where liftEq = genericLiftEq
instance Show1 BlockQuote where liftShowsPrec = genericLiftShowsPrec


-- Inline elements

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

data Link a = Link { linkURL :: ByteString, linkTitle :: Maybe ByteString }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Link where liftEq = genericLiftEq
instance Show1 Link where liftShowsPrec = genericLiftShowsPrec

data Image a = Image { imageURL :: ByteString, imageTitle :: Maybe ByteString }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Image where liftEq = genericLiftEq
instance Show1 Image where liftShowsPrec = genericLiftShowsPrec

data Code a = Code { codeLanguage :: Maybe ByteString, codeContent :: ByteString }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Code where liftEq = genericLiftEq
instance Show1 Code where liftShowsPrec = genericLiftShowsPrec
