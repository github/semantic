{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.TOCSummary
( Declaration(..)
, HasDeclaration
, declarationAlgebra
) where

import Prologue hiding (project)

import           Control.Arrow
import           Control.Rewriting hiding (apply)
import           Data.Blob
import           Data.Error (Error (..), showExpectation)
import           Data.Language as Language
import           Data.Location
import           Data.Range
import           Data.Source as Source
import qualified Data.Syntax as Syntax
import qualified Data.Syntax.Declaration as Declaration
import           Data.Term
import qualified Data.Text as T
import qualified Language.Markdown.Syntax as Markdown

-- | A declaration’s identifier and type.
data Declaration
  = MethodDeclaration   { declarationIdentifier :: Text, declarationText :: Text, declarationSpan :: Span, declarationLanguage :: Language, declarationReceiver :: Maybe Text }
  | FunctionDeclaration { declarationIdentifier :: Text, declarationText :: Text, declarationSpan :: Span, declarationLanguage :: Language }
  | HeadingDeclaration  { declarationIdentifier :: Text, declarationText :: Text, declarationSpan :: Span, declarationLanguage :: Language, declarationLevel :: Int }
  | ErrorDeclaration    { declarationIdentifier :: Text, declarationText :: Text, declarationSpan :: Span, declarationLanguage :: Language }
  deriving (Eq, Generic, Show)


-- | An r-algebra producing 'Just' a 'Declaration' for syntax nodes corresponding to high-level declarations, or 'Nothing' otherwise.
--
--   Customizing this for a given syntax type involves two steps:
--
--   1. Defining a 'CustomHasDeclaration' instance for the type.
--   2. Adding the type to the 'DeclarationStrategy' type family.
--
--   If you’re getting errors about missing a 'CustomHasDeclaration' instance for your syntax type, you probably forgot step 1.
--
--   If you’re getting 'Nothing' for your syntax node at runtime, you probably forgot step 2.
declarationAlgebra :: (Foldable syntax, HasDeclaration syntax)
                   => Blob -> RAlgebra (TermF syntax Location) (Term syntax Location) (Maybe Declaration)
declarationAlgebra blob (In ann syntax) = toDeclaration blob ann syntax

-- | Types for which we can produce a 'Declaration' in 'Maybe'. There is exactly one instance of this typeclass
class HasDeclaration syntax where
  toDeclaration :: (Foldable syntax) => Blob -> Location -> syntax (Term syntax Location, Maybe Declaration) -> Maybe Declaration

instance (HasDeclaration' syntax syntax) => HasDeclaration syntax where
  toDeclaration = toDeclaration'

-- | Types for which we can produce a 'Declaration' in 'Maybe'. There is exactly one instance of this typeclass; adding customized 'Declaration's for a new type is done by defining an instance of 'CustomHasDeclaration' instead.
--
--   This typeclass employs the Advanced Overlap techniques designed by Oleg Kiselyov & Simon Peyton Jones: https://wiki.haskell.org/GHC/AdvancedOverlap.
class HasDeclaration' whole syntax where
  -- | Compute a 'Declaration' for a syntax type using its 'CustomHasDeclaration' instance, if any, or else falling back to the default definition (which simply returns 'Nothing').
  toDeclaration' :: (Foldable whole) => Blob -> Location -> syntax (Term whole Location, Maybe Declaration) -> Maybe Declaration

-- | Define 'toDeclaration' using the 'CustomHasDeclaration' instance for a type if there is one or else use the default definition.
--
--   This instance determines whether or not there is an instance for @syntax@ by looking it up in the 'DeclarationStrategy' type family. Thus producing a 'Declaration' for a node requires both defining a 'CustomHasDeclaration' instance _and_ adding a definition for the type to the 'DeclarationStrategy' type family to return 'Custom'.
--
--   Note that since 'DeclarationStrategy' has a fallback case for its final entry, this instance will hold for all types of kind @* -> *@. Thus, this must be the only instance of 'HasDeclaration', as any other instance would be indistinguishable.
instance (DeclarationStrategy syntax ~ strategy, HasDeclarationWithStrategy strategy whole syntax) => HasDeclaration' whole syntax where
  toDeclaration' = toDeclarationWithStrategy (Proxy :: Proxy strategy)


-- | Types for which we can produce a customized 'Declaration'. This returns in 'Maybe' so that some values can be opted out (e.g. anonymous functions).
class CustomHasDeclaration whole syntax where
  -- | Produce a customized 'Declaration' for a given syntax node.
  customToDeclaration :: (Foldable whole) => Blob -> Location -> syntax (Term whole Location, Maybe Declaration) -> Maybe Declaration


-- | Produce a 'HeadingDeclaration' from the first line of the heading of a 'Markdown.Heading' node.
instance CustomHasDeclaration whole Markdown.Heading where
  customToDeclaration Blob{..} ann (Markdown.Heading level terms _)
    = Just $ HeadingDeclaration (headingText terms) mempty (locationSpan ann) blobLanguage level
    where headingText terms = getSource $ maybe (locationByteRange ann) sconcat (nonEmpty (headingByteRange <$> toList terms))
          headingByteRange (Term (In ann _), _) = locationByteRange ann
          getSource = firstLine . toText . flip Source.slice blobSource
          firstLine = T.takeWhile (/= '\n')

-- | Produce an 'ErrorDeclaration' for 'Syntax.Error' nodes.
instance CustomHasDeclaration whole Syntax.Error where
  customToDeclaration Blob{..} ann err@Syntax.Error{}
    = Just $ ErrorDeclaration (T.pack (formatTOCError (Syntax.unError (locationSpan ann) err))) mempty (locationSpan ann) blobLanguage
    where formatTOCError e = showExpectation False (errorExpected e) (errorActual e) ""

-- | Produce a 'FunctionDeclaration' for 'Declaration.Function' nodes so long as their identifier is non-empty (defined as having a non-empty 'Range').
instance CustomHasDeclaration whole Declaration.Function where
  customToDeclaration blob@Blob{..} ann decl@(Declaration.Function _ (Term (In identifierAnn _), _) _ _)
    -- Do not summarize anonymous functions
    | isEmpty identifierAnn = Nothing
    -- Named functions
    | otherwise             = Just $ FunctionDeclaration (getSource blobSource identifierAnn) functionSource (locationSpan ann) blobLanguage
    where isEmpty = (== 0) . rangeLength . locationByteRange
          functionSource = getIdentifier (arr Declaration.functionBody) blob (In ann decl)

-- | Produce a 'MethodDeclaration' for 'Declaration.Method' nodes. If the method’s receiver is non-empty (defined as having a non-empty 'Range'), the 'declarationIdentifier' will be formatted as 'receiver.method_name'; otherwise it will be simply 'method_name'.
instance CustomHasDeclaration whole Declaration.Method where
  customToDeclaration blob@Blob{..} ann decl@(Declaration.Method _ (Term (In receiverAnn receiverF), _) (Term (In identifierAnn _), _) _ _)
    -- Methods without a receiver
    | isEmpty receiverAnn = Just $ MethodDeclaration (getSource blobSource identifierAnn) methodSource (locationSpan ann) blobLanguage Nothing
    -- Methods with a receiver type and an identifier (e.g. (a *Type) in Go).
    | blobLanguage == Go
    , [ _, Term (In receiverType _) ] <- toList receiverF = Just $ MethodDeclaration (getSource blobSource identifierAnn) methodSource (locationSpan ann) blobLanguage (Just (getSource blobSource receiverType))
    -- Methods with a receiver (class methods) are formatted like `receiver.method_name`
    | otherwise           = Just $ MethodDeclaration (getSource blobSource identifierAnn) methodSource (locationSpan ann) blobLanguage (Just (getSource blobSource receiverAnn))
    where
      isEmpty = (== 0) . rangeLength . locationByteRange
      methodSource = getIdentifier (arr Declaration.methodBody) blob (In ann decl)

-- When encountering a Declaration-annotated term, we need to extract a Text
-- for the resulting Declaration's 'declarationIdentifier' field. This text
-- is constructed by slicing out text from the original blob corresponding
-- to a location, which is found via the passed-in rule.
getIdentifier :: Functor m
           => Rule () (m (Term syntax Location)) (Term syntax Location)
           -> Blob
           -> TermF m Location (Term syntax Location, a)
           -> Text
getIdentifier finder Blob{..} (In a r)
  = let declRange = locationByteRange a
        bodyRange = locationByteRange <$> rewrite (finder >>^ annotation) () (fmap fst r)
        -- Text-based gyrations to slice the identifier out of the provided blob source
        sliceFrom = T.stripEnd . toText . flip Source.slice blobSource . subtractRange declRange
    in either (const mempty) sliceFrom bodyRange

getSource :: Source -> Location -> Text
getSource blobSource = toText . flip Source.slice blobSource . locationByteRange

-- | Produce a 'Declaration' for 'Sum's using the 'HasDeclaration' instance & therefore using a 'CustomHasDeclaration' instance when one exists & the type is listed in 'DeclarationStrategy'.
instance Apply (HasDeclaration' whole) fs => CustomHasDeclaration whole (Sum fs) where
  customToDeclaration blob ann = apply @(HasDeclaration' whole) (toDeclaration' blob ann)


-- | A strategy for defining a 'HasDeclaration' instance. Intended to be promoted to the kind level using @-XDataKinds@.
data Strategy = Default | Custom

-- | Produce a 'Declaration' for a syntax node using either the 'Default' or 'Custom' strategy.
--
--   You should probably be using 'CustomHasDeclaration' instead of this class; and you should not define new instances of this class.
class HasDeclarationWithStrategy (strategy :: Strategy) whole syntax where
  toDeclarationWithStrategy :: (Foldable whole) => proxy strategy -> Blob -> Location -> syntax (Term whole Location, Maybe Declaration) -> Maybe Declaration


-- | A predicate on syntax types selecting either the 'Custom' or 'Default' strategy.
--
--   Only entries for which we want to use the 'Custom' strategy should be listed, with the exception of the final entry which maps all other types onto the 'Default' strategy.
--
--   If you’re seeing errors about missing a 'CustomHasDeclaration' instance for a given type, you’ve probably listed it in here but not defined a 'CustomHasDeclaration' instance for it, or else you’ve listed the wrong type in here. Conversely, if your 'customHasDeclaration' method is never being called, you may have forgotten to list the type in here.
type family DeclarationStrategy syntax where
  DeclarationStrategy Declaration.Function = 'Custom
  DeclarationStrategy Declaration.Method = 'Custom
  DeclarationStrategy Markdown.Heading = 'Custom
  DeclarationStrategy Syntax.Error = 'Custom
  DeclarationStrategy (Sum fs) = 'Custom
  DeclarationStrategy a = 'Default


-- | The 'Default' strategy produces 'Nothing'.
instance HasDeclarationWithStrategy 'Default whole syntax where
  toDeclarationWithStrategy _ _ _ _ = Nothing

-- | The 'Custom' strategy delegates the selection of the strategy to the 'CustomHasDeclaration' instance for the type.
instance CustomHasDeclaration whole syntax => HasDeclarationWithStrategy 'Custom whole syntax where
  toDeclarationWithStrategy _ = customToDeclaration
