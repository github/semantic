{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, RecordWildCards, TypeFamilies #-}
module Language.Markdown.Term
( Syntax
, Term(..)
) where

import Control.Lens.Lens
import Data.Abstract.Declarations
import Data.Aeson (ToJSON)
import Data.Bifunctor
import Data.Bitraversable
import Data.Coerce
import Data.Foldable (fold)
import Data.Functor.Foldable (Base, Recursive(..))
import qualified Data.Sum as Sum
import qualified Data.Syntax as Syntax
import qualified Data.Term as Term
import Data.Traversable
import Diffing.Interpreter
import qualified Language.Markdown.Syntax as Markup
import Source.Loc
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


newtype Term ann = Term { getTerm :: Term.TermF (Sum.Sum Syntax) ann (Term ann) }
  deriving (Declarations, Eq, Ord, Show, ToJSON)

instance Term.IsTerm Term where
  type Syntax Term = Sum.Sum Syntax
  toTermF = coerce
  fromTermF = coerce

instance Foldable Term where
  foldMap = foldMapDefault

instance Functor Term where
  fmap = fmapDefault

instance Traversable Term where
  traverse f = go where go = fmap Term . bitraverse f go . getTerm

instance Syntax.HasErrors Term where
  getErrors = cata $ \ (Term.In Loc{..} syntax) ->
    maybe (fold syntax) (pure . Syntax.unError span) (Sum.project syntax)


instance DiffTerms Term where
  diffTermPair = diffTermPair . bimap (cata Term.Term) (cata Term.Term)

type instance Base (Term ann) = Term.TermF (Sum.Sum Syntax) ann

instance Recursive (Term ann) where
  project = getTerm

instance HasSpan ann => HasSpan (Term ann) where
  span_ = inner.span_ where inner = lens getTerm (\t i -> t { getTerm = i })
  {-# INLINE span_ #-}
