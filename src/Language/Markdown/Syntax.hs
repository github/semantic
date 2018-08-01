{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Markdown.Syntax where

import Prologue hiding (Text)
import Data.JSON.Fields
import qualified Data.Text as T
import Diffing.Algorithm

newtype Document a = Document [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Document where liftEq = genericLiftEq
instance Ord1 Document where liftCompare = genericLiftCompare
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec


-- Block elements

newtype Paragraph a = Paragraph [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Ord1 Paragraph where liftCompare = genericLiftCompare
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec

data Heading a = Heading { headingLevel :: Int, headingContent :: [a], sectionContent :: [a] }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Heading where liftEq = genericLiftEq
instance Ord1 Heading where liftCompare = genericLiftCompare
instance Show1 Heading where liftShowsPrec = genericLiftShowsPrec

newtype UnorderedList a = UnorderedList [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 UnorderedList where liftEq = genericLiftEq
instance Ord1 UnorderedList where liftCompare = genericLiftCompare
instance Show1 UnorderedList where liftShowsPrec = genericLiftShowsPrec

newtype OrderedList a = OrderedList [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 OrderedList where liftEq = genericLiftEq
instance Ord1 OrderedList where liftCompare = genericLiftCompare
instance Show1 OrderedList where liftShowsPrec = genericLiftShowsPrec

newtype BlockQuote a = BlockQuote [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 BlockQuote where liftEq = genericLiftEq
instance Ord1 BlockQuote where liftCompare = genericLiftCompare
instance Show1 BlockQuote where liftShowsPrec = genericLiftShowsPrec

data ThematicBreak a = ThematicBreak
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 ThematicBreak where liftEq = genericLiftEq
instance Ord1 ThematicBreak where liftCompare = genericLiftCompare
instance Show1 ThematicBreak where liftShowsPrec = genericLiftShowsPrec

newtype HTMLBlock a = HTMLBlock T.Text
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 HTMLBlock where liftEq = genericLiftEq
instance Ord1 HTMLBlock where liftCompare = genericLiftCompare
instance Show1 HTMLBlock where liftShowsPrec = genericLiftShowsPrec

newtype Table a = Table [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Table where liftEq = genericLiftEq
instance Ord1 Table where liftCompare = genericLiftCompare
instance Show1 Table where liftShowsPrec = genericLiftShowsPrec

newtype TableRow a = TableRow [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 TableRow where liftEq = genericLiftEq
instance Ord1 TableRow where liftCompare = genericLiftCompare
instance Show1 TableRow where liftShowsPrec = genericLiftShowsPrec

newtype TableCell a = TableCell [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 TableCell where liftEq = genericLiftEq
instance Ord1 TableCell where liftCompare = genericLiftCompare
instance Show1 TableCell where liftShowsPrec = genericLiftShowsPrec


-- Inline elements

newtype Strong a = Strong [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Strong where liftEq = genericLiftEq
instance Ord1 Strong where liftCompare = genericLiftCompare
instance Show1 Strong where liftShowsPrec = genericLiftShowsPrec

newtype Emphasis a = Emphasis [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Emphasis where liftEq = genericLiftEq
instance Ord1 Emphasis where liftCompare = genericLiftCompare
instance Show1 Emphasis where liftShowsPrec = genericLiftShowsPrec

newtype Text a = Text T.Text
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Text where liftEq = genericLiftEq
instance Ord1 Text where liftCompare = genericLiftCompare
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

data Link a = Link { linkURL :: T.Text, linkTitle :: Maybe T.Text }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Link where liftEq = genericLiftEq
instance Ord1 Link where liftCompare = genericLiftCompare
instance Show1 Link where liftShowsPrec = genericLiftShowsPrec

data Image a = Image { imageURL :: T.Text, imageTitle :: Maybe T.Text }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Image where liftEq = genericLiftEq
instance Ord1 Image where liftCompare = genericLiftCompare
instance Show1 Image where liftShowsPrec = genericLiftShowsPrec

data Code a = Code { codeLanguage :: Maybe T.Text, codeContent :: T.Text }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Code where liftEq = genericLiftEq
instance Ord1 Code where liftCompare = genericLiftCompare
instance Show1 Code where liftShowsPrec = genericLiftShowsPrec

data LineBreak a = LineBreak
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 LineBreak where liftEq = genericLiftEq
instance Ord1 LineBreak where liftCompare = genericLiftCompare
instance Show1 LineBreak where liftShowsPrec = genericLiftShowsPrec

newtype Strikethrough a = Strikethrough [a]
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1)

instance Eq1 Strikethrough where liftEq = genericLiftEq
instance Ord1 Strikethrough where liftCompare = genericLiftCompare
instance Show1 Strikethrough where liftShowsPrec = genericLiftShowsPrec
