{-# LANGUAGE DeriveAnyClass, DerivingVia, DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Language.Markdown.Syntax where

import           Data.Abstract.Declarations
import           Data.JSON.Fields
import qualified Data.Text as T
import           Diffing.Algorithm
import           Prologue hiding (Text)
import           Proto3.Suite
import qualified Proto3.Suite as PB

newtype Document a = Document { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Document


-- Block elements

newtype Paragraph a = Paragraph { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Paragraph

data Heading a = Heading { headingLevel :: Int, headingContent :: [a], sectionContent :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Heading

newtype UnorderedList a = UnorderedList { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically UnorderedList

newtype OrderedList a = OrderedList { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically OrderedList

newtype BlockQuote a = BlockQuote { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically BlockQuote

data ThematicBreak a = ThematicBreak
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically ThematicBreak

newtype HTMLBlock a = HTMLBlock { value :: T.Text }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically HTMLBlock

newtype Table a = Table { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Table

newtype TableRow a = TableRow { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TableRow

newtype TableCell a = TableCell { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically TableCell


-- Inline elements

newtype Strong a = Strong { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Strong

newtype Emphasis a = Emphasis { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Emphasis

newtype Text a = Text { value :: T.Text}
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Text

data Link a = Link { linkURL :: T.Text, linkTitle :: Maybe T.Text }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Link

instance Message1 Link where
  liftEncodeMessage _ _ Link{..} = encodeMessageField 1 linkURL <> maybe mempty (encodeMessageField 2) linkTitle
  liftDecodeMessage = undefined
  liftDotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim PB.String) (Single "linkUrl") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "linkTitle") [] Nothing
    ]

data Image a = Image { imageURL :: T.Text, imageTitle :: Maybe T.Text }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Image

instance Message1 Image where
  liftEncodeMessage _ _ Image{..} = encodeMessageField 1 imageURL <> maybe mempty (encodeMessageField 2) imageTitle
  liftDecodeMessage = undefined
  liftDotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim PB.String) (Single "imageURL") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "imageTitle") [] Nothing
    ]

data Code a = Code { codeLanguage :: Maybe T.Text, codeContent :: T.Text }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Code

instance Message1 Code where
  liftEncodeMessage _ _ Code{..} = maybe mempty (encodeMessageField 1) codeLanguage <> encodeMessageField 2 codeContent
  liftDecodeMessage = undefined
  liftDotProto _ =
    [ DotProtoMessageField $ DotProtoField 1 (Prim PB.String) (Single "codeLanguage") [] Nothing
    , DotProtoMessageField $ DotProtoField 2 (Prim PB.String) (Single "codeContent") [] Nothing
    ]

data LineBreak a = LineBreak
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically LineBreak

newtype Strikethrough a = Strikethrough { values :: [a] }
  deriving (Eq, Ord, Show, Declarations1, Foldable, Traversable, Functor, Generic1, Hashable1, Diffable, ToJSONFields1, Named1, Message1, NFData1)
  deriving (Eq1, Show1, Ord1) via Generically Strikethrough
