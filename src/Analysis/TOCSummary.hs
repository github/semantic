{-# LANGUAGE AllowAmbiguousTypes, DataKinds, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses, OverloadedStrings, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances, ViewPatterns #-}
module Analysis.TOCSummary
( Declaration(..)
, formatIdentifier
, Kind(..)
, formatKind
, HasDeclaration
, declarationAlgebra
) where

import Prologue hiding (project)

import           Data.Blob
import qualified Data.Error as Error
import           Data.Flag
import           Data.Language as Language
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import           Data.Term
import qualified Data.Text as T
import qualified Language.Markdown.Syntax as Markdown
import           Source.Loc as Loc
import           Source.Range
import           Source.Source as Source

-- | A declaration’s identifier and type.
data Declaration = Declaration
  { kind       :: Kind
  , identifier :: Text
  , span       :: Span
  , language   :: Language
  }
  deriving (Eq, Show)

formatIdentifier :: Declaration -> Text
formatIdentifier (Declaration kind identifier _ lang) = case kind of
  Method (Just receiver)
    | Language.Go <- lang -> "(" <> receiver <> ") " <> identifier
    | otherwise           -> receiver <> "." <> identifier
  _                       -> identifier

data Kind
  = Method (Maybe Text)
  | Function
  | Heading Int
  | Error
  deriving (Eq, Ord, Show)

formatKind :: Kind -> T.Text
formatKind = \case
  Function  -> "Function"
  Method _  -> "Method"
  Heading l -> "Heading " <> T.pack (show l)
  Error     -> "ParseError"


-- | An r-algebra producing 'Just' a 'Declaration' for syntax nodes corresponding to high-level declarations, or 'Nothing' otherwise.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a @'HasDeclarationBy' ''Custom'@ instance for the type.
--   2. Adding the type to the 'DeclarationStrategy' type family.
--
--   If you’re getting errors about missing a @'HasDeclarationBy' ''Custom'@ instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
declarationAlgebra :: (Foldable (Syntax term), HasDeclaration (Syntax term), IsTerm term)
                   => Blob -> RAlgebra (TermF (Syntax term) Loc) (term Loc) (Maybe Declaration)
declarationAlgebra blob (In ann syntax) = toDeclaration blob ann syntax

-- | Types for which we can produce a 'Declaration' in 'Maybe'. There is exactly one instance of this typeclass; adding customized 'Declaration's for a new type is done by defining an instance of @'HasDeclarationBy' ''Custom'@ instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasDeclaration syntax where
  -- | Compute a 'Declaration' for a syntax type using its @'HasDeclarationBy' ''Custom'@ instance, if any, or else falling back to the default definition (which simply returns 'Nothing').
  toDeclaration :: (Foldable (Syntax term), IsTerm term) => Blob -> Loc -> syntax (term Loc, Maybe Declaration) -> Maybe Declaration

-- | Define 'toDeclaration' using the @'HasDeclarationBy' ''Custom'@ instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'DeclarationStrategy' type family. Thus producing a 'Declaration' for a node requires both defining a @'HasDeclarationBy' ''Custom'@ instance _and_ adding a definition for the type to the 'DeclarationStrategy' type family to return 'Custom'.
--
--   Note that since 'DeclarationStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasDeclaration', as any other instance would be indistinguishable.
instance (DeclarationStrategy syntax ~ strategy, HasDeclarationBy strategy syntax) => HasDeclaration syntax where
  toDeclaration = toDeclarationBy @strategy


-- | Produce a 'Declaration' for a syntax node using either the 'Default' or 'Custom' strategy.
class HasDeclarationBy (strategy :: Strategy) syntax where
  toDeclarationBy :: (Foldable (Syntax term), IsTerm term) => Blob -> Loc -> syntax (term Loc, Maybe Declaration) -> Maybe Declaration

-- | The 'Default' strategy produces 'Nothing'.
instance HasDeclarationBy 'Default syntax where
  toDeclarationBy _ _ _ = Nothing


-- | Produce a 'Heading' from the first line of the heading of a 'Markdown.Heading' node.
instance HasDeclarationBy 'Custom Markdown.Heading where
  toDeclarationBy blob@Blob{..} ann (Markdown.Heading level terms _)
    = Just $ Declaration (Heading level) (headingText terms) (Loc.span ann) (blobLanguage blob)
    where headingText terms = getSource $ maybe (byteRange ann) sconcat (nonEmpty (headingByteRange <$> toList terms))
          headingByteRange (t, _) = byteRange (termAnnotation t)
          getSource = firstLine . toText . Source.slice blobSource
          firstLine = T.takeWhile (/= '\n')

-- | Produce an 'Error' for 'Syntax.Error' nodes.
instance HasDeclarationBy 'Custom Syntax.Error where
  toDeclarationBy blob@Blob{..} ann err@Syntax.Error{}
    = Just $ Declaration Error (T.pack (formatTOCError (Syntax.unError (Loc.span ann) err))) (Loc.span ann) (blobLanguage blob)
    where formatTOCError e = Error.showExpectation (flag Error.Colourize False) (Error.errorExpected e) (Error.errorActual e) ""

-- | Produce a 'Function' for 'Declaration.Function' nodes so long as their identifier is non-empty (defined as having a non-empty 'Range').
instance HasDeclarationBy 'Custom Declaration.Function where
  toDeclarationBy blob@Blob{..} ann (Declaration.Function _ (termAnnotation -> identifierAnn, _) _ _)
    -- Do not summarize anonymous functions
    | isEmpty identifierAnn = Nothing
    -- Named functions
    | otherwise             = Just $ Declaration Function (getSource blobSource identifierAnn) (Loc.span ann) (blobLanguage blob)
    where isEmpty = (== 0) . rangeLength . byteRange

-- | Produce a 'Method' for 'Declaration.Method' nodes. If the method’s receiver is non-empty (defined as having a non-empty 'Range'), the 'identifier' will be formatted as 'receiver.method_name'; otherwise it will be simply 'method_name'.
instance HasDeclarationBy 'Custom Declaration.Method where
  toDeclarationBy blob@Blob{..} ann (Declaration.Method _ (toTermF -> In receiverAnn receiverF, _) (termAnnotation -> identifierAnn, _) _ _ _)
    -- Methods without a receiver
    | isEmpty receiverAnn = Just $ Declaration (Method Nothing) (getSource blobSource identifierAnn) (Loc.span ann) (blobLanguage blob)
    -- Methods with a receiver type and an identifier (e.g. (a *Type) in Go).
    | blobLanguage blob == Go
    , [ _, termAnnotation -> receiverType ] <- toList receiverF = Just $ Declaration (Method (Just (getSource blobSource receiverType))) (getSource blobSource identifierAnn) (Loc.span ann) (blobLanguage blob)
    -- Methods with a receiver (class methods) are formatted like `receiver.method_name`
    | otherwise           = Just $ Declaration (Method (Just (getSource blobSource receiverAnn))) (getSource blobSource identifierAnn) (Loc.span ann) (blobLanguage blob)
    where
      isEmpty = (== 0) . rangeLength . byteRange

getSource :: Source -> Loc -> Text
getSource blobSource = toText . Source.slice blobSource . byteRange

-- | Produce a 'Declaration' for 'Sum's using the 'HasDeclaration' instance & therefore using a @'HasDeclarationBy' ''Custom'@ instance when one exists & the type is listed in 'DeclarationStrategy'.
instance Apply HasDeclaration fs => HasDeclarationBy 'Custom (Sum fs) where
  toDeclarationBy blob ann = apply @HasDeclaration (toDeclaration blob ann)


-- | A strategy for defining a 'HasDeclaration' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a @'HasDeclarationBy' ''Custom'@ instance for a given type, you’ve probably listed it in here but not defined a @'HasDeclarationBy' ''Custom'@ instance for it, or else you’ve listed the wrong type in here. Conversely, if your @'HasDeclarationBy' ''Custom'@ method is never being called, you may have forgotten to list the type in here.
type family DeclarationStrategy syntax where
  DeclarationStrategy Declaration.Function = 'Custom
  DeclarationStrategy Declaration.Method   = 'Custom
  DeclarationStrategy Markdown.Heading     = 'Custom
  DeclarationStrategy Syntax.Error         = 'Custom
  DeclarationStrategy (Sum _)              = 'Custom
  DeclarationStrategy _                    = 'Default
