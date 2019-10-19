{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, TypeFamilies #-}
module Language.Markdown.Term
( Syntax
, Term(..)
) where

import Control.Lens.Lens
import Data.Bifunctor
import Data.Diff
import Data.Functor.Foldable
import Data.Sum (Sum)
import qualified Data.Syntax as Syntax
import qualified Data.Term as Term
import Diffing.Interpreter
import qualified Language.Markdown.Syntax as Markup
import Source.Span

type Syntax =
  [ Markup.Document
  -- Block elements
  , Markup.BlockQuote
  , Markup.Heading
  , Markup.HTMLBlock
  , Markup.OrderedList
  , Markup.Paragraph
  , Markup.ThematicBreak
  , Markup.UnorderedList
  , Markup.Table
  , Markup.TableRow
  , Markup.TableCell
  -- Inline elements
  , Markup.Code
  , Markup.Emphasis
  , Markup.Image
  , Markup.LineBreak
  , Markup.Link
  , Markup.Strong
  , Markup.Text
  , Markup.Strikethrough
  -- Assignment errors; cmark does not provide parse errors.
  , Syntax.Error
  , []
  ]


newtype Term ann = Term { getTerm :: Term.Term (Sum Syntax) ann }
  deriving (Eq, Foldable, Functor, Syntax.HasErrors, Ord, Show, Traversable)

instance DiffTerms Term where
  type DiffFor Term = Diff (Sum Syntax)
  diffTermPair = diffTermPair . bimap getTerm getTerm

type instance Base (Term ann) = Term.TermF (Sum Syntax) ann

instance Recursive (Term ann) where
  project = fmap Term . project . getTerm

instance HasSpan ann => HasSpan (Term ann) where
  span_ = inner.span_ where inner = lens getTerm (\t i -> t { getTerm = i })
  {-# INLINE span_ #-}
