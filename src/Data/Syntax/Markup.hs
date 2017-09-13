{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Markup where

import Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Show.Generic
import GHC.Generics


newtype Document a = Document [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Document where liftEq = genericLiftEq
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec


-- Block elements

newtype Paragraph a = Paragraph [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec

data Section a = Section { sectionLevel :: Int, sectionHeading :: a, sectionContent :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Section where liftEq = genericLiftEq
instance Show1 Section where liftShowsPrec = genericLiftShowsPrec

data Heading a = Heading { headingLevel :: Int, headingContent :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Heading where liftEq = genericLiftEq
instance Show1 Heading where liftShowsPrec = genericLiftShowsPrec

newtype UnorderedList a = UnorderedList [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 UnorderedList where liftEq = genericLiftEq
instance Show1 UnorderedList where liftShowsPrec = genericLiftShowsPrec

newtype OrderedList a = OrderedList [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 OrderedList where liftEq = genericLiftEq
instance Show1 OrderedList where liftShowsPrec = genericLiftShowsPrec

newtype BlockQuote a = BlockQuote [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 BlockQuote where liftEq = genericLiftEq
instance Show1 BlockQuote where liftShowsPrec = genericLiftShowsPrec

data ThematicBreak a = ThematicBreak
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ThematicBreak where liftEq = genericLiftEq
instance Show1 ThematicBreak where liftShowsPrec = genericLiftShowsPrec

data HTMLBlock a = HTMLBlock ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 HTMLBlock where liftEq = genericLiftEq
instance Show1 HTMLBlock where liftShowsPrec = genericLiftShowsPrec

newtype Table a = Table [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Table where liftEq = genericLiftEq
instance Show1 Table where liftShowsPrec = genericLiftShowsPrec

newtype TableRow a = TableRow [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TableRow where liftEq = genericLiftEq
instance Show1 TableRow where liftShowsPrec = genericLiftShowsPrec

newtype TableCell a = TableCell [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TableCell where liftEq = genericLiftEq
instance Show1 TableCell where liftShowsPrec = genericLiftShowsPrec


-- Inline elements

newtype Strong a = Strong [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Strong where liftEq = genericLiftEq
instance Show1 Strong where liftShowsPrec = genericLiftShowsPrec

newtype Emphasis a = Emphasis [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Emphasis where liftEq = genericLiftEq
instance Show1 Emphasis where liftShowsPrec = genericLiftShowsPrec

newtype Text a = Text ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

data Link a = Link { linkURL :: ByteString, linkTitle :: Maybe ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Link where liftEq = genericLiftEq
instance Show1 Link where liftShowsPrec = genericLiftShowsPrec

data Image a = Image { imageURL :: ByteString, imageTitle :: Maybe ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Image where liftEq = genericLiftEq
instance Show1 Image where liftShowsPrec = genericLiftShowsPrec

data Code a = Code { codeLanguage :: Maybe ByteString, codeContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Code where liftEq = genericLiftEq
instance Show1 Code where liftShowsPrec = genericLiftShowsPrec

data LineBreak a = LineBreak
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 LineBreak where liftEq = genericLiftEq
instance Show1 LineBreak where liftShowsPrec = genericLiftShowsPrec

newtype Strikethrough a = Strikethrough [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Strikethrough where liftEq = genericLiftEq
instance Show1 Strikethrough where liftShowsPrec = genericLiftShowsPrec
