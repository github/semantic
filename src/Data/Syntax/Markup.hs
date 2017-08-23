{-# LANGUAGE DeriveAnyClass #-}
module Data.Syntax.Markup where

import Algorithm
import Data.Align.Generic
import Data.ByteString (ByteString)
import Data.Functor.Classes.Eq.Generic
import Data.Functor.Classes.Pretty.Generic
import Data.Functor.Classes.Show.Generic
import Data.Maybe (catMaybes)
import Data.Text.Encoding (decodeUtf8With)
import GHC.Generics


newtype Document a = Document [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Document where liftEq = genericLiftEq
instance Show1 Document where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Document where liftPretty = genericLiftPretty


-- Block elements

newtype Paragraph a = Paragraph [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Paragraph where liftEq = genericLiftEq
instance Show1 Paragraph where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Paragraph where liftPretty = genericLiftPretty

data Section a = Section { sectionLevel :: Int, sectionHeading :: a, sectionContent :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Section where liftEq = genericLiftEq
instance Show1 Section where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Section where liftPretty = genericLiftPretty

data Heading a = Heading { headingLevel :: Int, headingContent :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Heading where liftEq = genericLiftEq
instance Show1 Heading where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Heading where liftPretty = genericLiftPretty

newtype UnorderedList a = UnorderedList [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 UnorderedList where liftEq = genericLiftEq
instance Show1 UnorderedList where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 UnorderedList where liftPretty = genericLiftPretty

newtype OrderedList a = OrderedList [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 OrderedList where liftEq = genericLiftEq
instance Show1 OrderedList where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 OrderedList where liftPretty = genericLiftPretty

newtype BlockQuote a = BlockQuote [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 BlockQuote where liftEq = genericLiftEq
instance Show1 BlockQuote where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 BlockQuote where liftPretty = genericLiftPretty

data ThematicBreak a = ThematicBreak
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 ThematicBreak where liftEq = genericLiftEq
instance Show1 ThematicBreak where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 ThematicBreak where liftPretty = genericLiftPretty

data HTMLBlock a = HTMLBlock ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 HTMLBlock where liftEq = genericLiftEq
instance Show1 HTMLBlock where liftShowsPrec = genericLiftShowsPrec

instance Pretty1 HTMLBlock where
  liftPretty _ _ (HTMLBlock s) = pretty ("HTMLBlock" :: String) <+> prettyBytes s

newtype Table a = Table [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Table where liftEq = genericLiftEq
instance Show1 Table where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Table where liftPretty = genericLiftPretty

newtype TableRow a = TableRow [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TableRow where liftEq = genericLiftEq
instance Show1 TableRow where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 TableRow where liftPretty = genericLiftPretty

newtype TableCell a = TableCell [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 TableCell where liftEq = genericLiftEq
instance Show1 TableCell where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 TableCell where liftPretty = genericLiftPretty


-- Inline elements

newtype Strong a = Strong [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Strong where liftEq = genericLiftEq
instance Show1 Strong where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Strong where liftPretty = genericLiftPretty

newtype Emphasis a = Emphasis [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Emphasis where liftEq = genericLiftEq
instance Show1 Emphasis where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Emphasis where liftPretty = genericLiftPretty

newtype Text a = Text ByteString
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Text where liftEq = genericLiftEq
instance Show1 Text where liftShowsPrec = genericLiftShowsPrec

instance Pretty1 Text where
  liftPretty _ _ (Text s) = pretty ("Text" :: String) <+> prettyBytes s

data Link a = Link { linkURL :: ByteString, linkTitle :: Maybe ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Link where liftEq = genericLiftEq
instance Show1 Link where liftShowsPrec = genericLiftShowsPrec

instance Pretty1 Link where
  liftPretty _ _ (Link u t) = pretty ("Link" :: String) <+> prettyBytes u <+> liftPretty prettyBytes (list . map prettyBytes) t

data Image a = Image { imageURL :: ByteString, imageTitle :: Maybe ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Image where liftEq = genericLiftEq
instance Show1 Image where liftShowsPrec = genericLiftShowsPrec

instance Pretty1 Image where
  liftPretty _ _ (Image u t) = pretty ("Image" :: String) <+> prettyBytes u <+> liftPretty prettyBytes (list . map prettyBytes) t

data Code a = Code { codeLanguage :: Maybe ByteString, codeContent :: ByteString }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Code where liftEq = genericLiftEq
instance Show1 Code where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Code where
  liftPretty _ _ (Code l c) = nest 2 (vsep (catMaybes [Just (pretty ("Code" :: String)), fmap prettyBytes l, Just (prettyBytes c)]))

data LineBreak a = LineBreak
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 LineBreak where liftEq = genericLiftEq
instance Show1 LineBreak where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 LineBreak where liftPretty = genericLiftPretty

newtype Strikethrough a = Strikethrough [a]
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Show, Traversable)

instance Eq1 Strikethrough where liftEq = genericLiftEq
instance Show1 Strikethrough where liftShowsPrec = genericLiftShowsPrec
instance Pretty1 Strikethrough where liftPretty = genericLiftPretty

prettyBytes :: ByteString -> Doc ann
prettyBytes = pretty . decodeUtf8With (\ _ -> ('\xfffd' <$))
