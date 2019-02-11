module Data.Abstract.HasSpan (HasSpan(..)) where

import Data.Location
import Data.Quieterm
import Data.Term


class HasSpan term where
  getSpan :: term -> Span


instance HasSpan ann => HasSpan (Term syntax ann) where
  getSpan = getSpan . termAnnotation

instance HasSpan ann => HasSpan (Quieterm syntax ann) where
  getSpan = getSpan . termFAnnotation . unQuieterm

instance HasSpan Location where
  getSpan = locationSpan
