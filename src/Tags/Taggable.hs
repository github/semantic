{- |

Taggable allows projecting syntax terms to a list of named symbols. In order to
identify a new syntax as Taggable, you need to:

1. Give that syntax a non-derived @TaggableBy 'Custom@ instance and implement at least the
'symbolName'' method.

2. Add an equation to 'TaggableInstance' for the type with the value ''Custom'.

3. Make sure that 'symbolsToSummarize' in Tagging.hs includes the string
constructor name of this syntax.

-}
{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, KindSignatures, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module Tags.Taggable
( Tagger
, Token(..)
, Taggable(..)
, IsTaggable
, HasTextElement
, subtractLocation
, tagging
)
where

import Prologue

import Analysis.ConstructorName
import Analysis.HasTextElement
import Data.Abstract.Declarations
import Data.Abstract.Name
import Data.Blob
import Data.Language
import Data.Location
import Data.Range
import Data.Term
import Data.Text hiding (empty)

import Streaming hiding (Sum)
import Streaming.Prelude (yield)

import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import qualified Data.Syntax.Expression as Expression
import qualified Language.Ruby.Syntax as Ruby
import qualified Language.TypeScript.Syntax as TypeScript


 -- TODO: Move to src/Data
data Token
  = Enter { tokenName :: Text, tokenSnippetRange :: Maybe Range }
  | Exit  { tokenName :: Text, tokenSnippetRange :: Maybe Range}
  | Iden  { identifierName :: Text, tokenSpan :: Span, docsLiteralRange :: Maybe Range }
  deriving (Eq, Show)

type Tagger = Stream (Of Token)

enter, exit :: Monad m => String -> Maybe Range -> Tagger m ()
enter c = yield . Enter (pack c)
exit c = yield . Exit (pack c)

emitIden :: Monad m => Span -> Maybe Range -> Name -> Tagger m ()
emitIden span docsLiteralRange name = yield (Iden (formatName name) span docsLiteralRange)

class Taggable constr where
  docsLiteral ::
    ( Foldable syntax
    , HasTextElement syntax
    )
    => Language -> constr (Term syntax Location) -> Maybe Range
  docsLiteral _ _ = Nothing

  snippet :: Foldable syntax => Location -> constr (Term syntax Location) -> Maybe Range
  snippet _ _ = Nothing

  symbolName :: Declarations1 syntax => constr (Term syntax Location) -> Maybe Name
  symbolName _ = Nothing

data Strategy = Default | Custom

class TaggableBy (strategy :: Strategy) constr where
  docsLiteral' ::
    ( Foldable syntax
    , HasTextElement syntax
    )
    => Language -> constr (Term syntax Location) -> Maybe Range
  docsLiteral' _ _ = Nothing

  snippet' :: (Foldable syntax) => Location -> constr (Term syntax Location) -> Maybe Range
  snippet' _ _ = Nothing

  symbolName' :: Declarations1 syntax => constr (Term syntax Location) -> Maybe Name
  symbolName' _ = Nothing

type IsTaggable syntax =
  ( Functor syntax
  , Foldable syntax
  , Taggable syntax
  , ConstructorName syntax
  , Declarations1 syntax
  , HasTextElement syntax
  )

tagging :: (Monad m, IsTaggable syntax)
        => Blob
        -> Term syntax Location
        -> Stream (Of Token) m ()
tagging b = foldSubterms (descend (blobLanguage b))

descend ::
  ( ConstructorName (TermF syntax Location)
  , IsTaggable syntax
  , Monad m
  )
  => Language -> SubtermAlgebra (TermF syntax Location) (Term syntax Location) (Tagger m ())
descend lang t@(In loc _) = do
  let term = fmap subterm t
  let snippetRange = snippet loc term
  let litRange = docsLiteral lang term

  enter (constructorName term) snippetRange
  maybe (pure ()) (emitIden (locationSpan loc) litRange) (symbolName term)
  traverse_ subtermRef t
  exit (constructorName term) snippetRange

subtractLocation :: Location -> Location -> Range
subtractLocation a b = subtractRange (locationByteRange a) (locationByteRange b)

-- Instances

instance (TaggableBy strategy t, strategy ~ TaggableInstance t) => Taggable t where
  docsLiteral = docsLiteral' @strategy
  snippet = snippet' @strategy
  symbolName = symbolName' @strategy

type family TaggableInstance (t :: * -> *) :: Strategy where
  TaggableInstance (Sum _)              = 'Custom
  TaggableInstance (TermF _ _)          = 'Custom
  TaggableInstance Syntax.Context       = 'Custom
  TaggableInstance Declaration.Function = 'Custom
  TaggableInstance Declaration.Method   = 'Custom
  TaggableInstance Declaration.Class    = 'Custom
  TaggableInstance Ruby.Class           = 'Custom
  TaggableInstance Ruby.Module          = 'Custom
  TaggableInstance TypeScript.Module    = 'Custom
  TaggableInstance Expression.Call      = 'Custom
  TaggableInstance Ruby.Send            = 'Custom
  TaggableInstance _                    = 'Default

instance TaggableBy 'Default t

instance Apply Taggable fs => TaggableBy 'Custom (Sum fs) where
  docsLiteral' a = apply @Taggable (docsLiteral a)
  snippet' x = apply @Taggable (snippet x)
  symbolName' = apply @Taggable symbolName

instance Taggable a => TaggableBy 'Custom (TermF a Location) where
  docsLiteral' l t = docsLiteral l (termFOut t)
  snippet' ann t = snippet ann (termFOut t)
  symbolName' t = symbolName (termFOut t)

instance TaggableBy 'Custom Syntax.Context where
  snippet' ann (Syntax.Context _ (Term (In subj _))) = Just (subtractLocation ann subj)

instance TaggableBy 'Custom Declaration.Function where
  docsLiteral' Python (Declaration.Function _ _ _ (Term (In _ bodyF)))
    | (Term (In exprAnn exprF):_) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise           = Nothing
  docsLiteral' _ _         = Nothing
  snippet' ann (Declaration.Function _ _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName' = declaredName . Declaration.functionName

instance TaggableBy 'Custom Declaration.Method where
  docsLiteral' Python (Declaration.Method _ _ _ _ (Term (In _ bodyF)) _)
    | (Term (In exprAnn exprF):_) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise           = Nothing
  docsLiteral' _ _         = Nothing
  snippet' ann (Declaration.Method _ _ _ _ (Term (In body _)) _) = Just $ subtractLocation ann body
  symbolName' = declaredName . Declaration.methodName

instance TaggableBy 'Custom Declaration.Class where
  docsLiteral' Python (Declaration.Class _ _ _ (Term (In _ bodyF)))
    | (Term (In exprAnn exprF):_) <- toList bodyF
    , isTextElement exprF = Just (locationByteRange exprAnn)
    | otherwise           = Nothing
  docsLiteral' _ _         = Nothing
  snippet' ann (Declaration.Class _ _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName' = declaredName . Declaration.classIdentifier

instance TaggableBy 'Custom Ruby.Class where
  snippet' ann (Ruby.Class _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName' = declaredName . Ruby.classIdentifier

instance TaggableBy 'Custom Ruby.Module where
  snippet' ann (Ruby.Module _ (Term (In body _):_)) = Just $ subtractLocation ann body
  snippet' ann (Ruby.Module _ _)                    = Just $ locationByteRange ann
  symbolName' = declaredName . Ruby.moduleIdentifier

instance TaggableBy 'Custom TypeScript.Module where
  snippet' ann (TypeScript.Module _ (Term (In body _):_)) = Just $ subtractLocation ann body
  snippet' ann (TypeScript.Module _ _                   ) = Just $ locationByteRange ann
  symbolName' = declaredName . TypeScript.moduleIdentifier

instance TaggableBy 'Custom Expression.Call where
  snippet' ann (Expression.Call _ _ _ (Term (In body _))) = Just $ subtractLocation ann body
  symbolName' = declaredName . Expression.callFunction

instance TaggableBy 'Custom Ruby.Send where
  snippet' ann (Ruby.Send _ _ _ (Just (Term (In body _)))) = Just $ subtractLocation ann body
  snippet' ann _                                           = Just $ locationByteRange ann
  symbolName' Ruby.Send{..} = declaredName =<< sendSelector
