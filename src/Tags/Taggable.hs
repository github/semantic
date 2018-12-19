{- |

Taggable allows projecting syntax terms to a list of named symbols. In order to
identify a new syntax as Taggable, you need to:

1. Give that syntax a non-derived Taggable instance and implement as least the
'symbolName' method.

2. Make sure that 'symbolsToSummarize' in Tagging.hs includes the string
constructor name of this syntax.

-}
{-# LANGUAGE AllowAmbiguousTypes, GADTs, ConstraintKinds, LambdaCase, RankNTypes, TypeFamilies, TypeOperators, ScopedTypeVariables, UndecidableInstances #-}
module Tags.Taggable
( Tagger
, Token(..)
, Taggable(..)
, IsTaggable
, HasTextElement
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
import Data.Machine as Machine
import Data.Term
import Data.Text hiding (empty)

-- TODO: Move to src/Data
data Token
  = Enter { tokenName :: Text, tokenSnippetRange :: Maybe Range }
  | Exit  { tokenName :: Text, tokenSnippetRange :: Maybe Range}
  | Iden  { identifierName :: Text, tokenSpan :: Span, docsLiteralRange :: Maybe Range }
  deriving (Eq, Show)

data Tagger a where
  Pure :: a -> Tagger a
  Bind :: Tagger a -> (a -> Tagger b) -> Tagger b
  Tell :: Token -> Tagger ()

compile :: Tagger a -> Machine.Plan k Token a
compile = \case
  Pure a   -> pure a
  Bind a f -> compile a >>= compile . f
  Tell t   -> Machine.yield t $> ()

instance Functor Tagger where fmap = liftA

instance Applicative Tagger where
  pure  = Pure
  (<*>) = ap

instance Monad Tagger where (>>=) = Bind

enter, exit :: String -> Maybe Range -> Tagger ()
enter c = Tell . Enter (pack c)
exit c = Tell . Exit (pack c)

emitIden :: Span -> Maybe Range -> Name -> Tagger ()
emitIden span docsLiteralRange name = Tell (Iden (formatName name) span docsLiteralRange)

class (Show1 constr, Traversable constr) => Taggable constr where
  docsLiteral ::
    ( Functor syntax
    , Foldable syntax
    , HasTextElement syntax
    )
    => Language -> constr (Term syntax Location) -> Maybe Range
  docsLiteral _ _ = Nothing

  snippet :: (Foldable syntax) => Location -> constr (Term syntax Location) -> Maybe Range
  snippet _ _ = Nothing

  symbolName :: Declarations1 syntax => constr (Term syntax Location) -> Maybe Name
  symbolName _ = Nothing

type IsTaggable syntax =
  ( Functor syntax
  , Foldable syntax
  , Traversable syntax
  , Show1 syntax
  , Taggable syntax
  , ConstructorName syntax
  , Declarations1 syntax
  , HasTextElement syntax
  )

tagging :: (IsTaggable syntax)
  => Blob
  -> Term syntax Location
  -> Machine.Source Token
tagging Blob{..} term = pipe
  where pipe = Machine.construct $ compile go
        go   = foldSubterms (descend blobLanguage) term

descend ::
  ( Taggable (TermF syntax Location)
  , ConstructorName (TermF syntax Location)
  , Functor syntax
  , Foldable syntax
  , HasTextElement syntax
  , Declarations1 syntax
  )
  => Language -> SubtermAlgebra (TermF syntax Location) (Term syntax Location) (Tagger ())
descend lang t@(In loc _) = do
  let term = fmap subterm t
  let snippetRange = snippet loc term
  let litRange = docsLiteral lang term

  enter (constructorName term) snippetRange
  maybe (pure ()) (emitIden (locationSpan loc) litRange) (symbolName term)
  traverse_ subtermRef t
  exit (constructorName term) snippetRange

-- Instances

instance (Taggable a) => Taggable (TermF a Location) where
  docsLiteral l t = docsLiteral l (termFOut t)
  snippet ann t = snippet ann (termFOut t)
  symbolName t = symbolName (termFOut t)
