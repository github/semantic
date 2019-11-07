{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Analysis.HasTextElement
( HasTextElement(..)
) where

import Data.Sum
import Prologue
import qualified Data.Syntax.Literal as Literal

class HasTextElement syntax where
  isTextElement :: syntax a -> Bool

instance (TextElementStrategy syntax ~ strategy, HasTextElementWithStrategy strategy syntax) => HasTextElement syntax where
  isTextElement = isTextElementWithStrategy (Proxy :: Proxy strategy)

class CustomHasTextElement syntax where
  customIsTextElement :: syntax a -> Bool

instance CustomHasTextElement Literal.TextElement where
  customIsTextElement _ = True

instance Apply HasTextElement fs => CustomHasTextElement (Sum fs) where
  customIsTextElement = apply @HasTextElement isTextElement

data Strategy = Default | Custom

class HasTextElementWithStrategy (strategy :: Strategy) syntax where
  isTextElementWithStrategy :: proxy strategy -> syntax a -> Bool

type family TextElementStrategy syntax where
  TextElementStrategy Literal.TextElement = 'Custom
  TextElementStrategy (Sum _) = 'Custom
  TextElementStrategy _ = 'Default

instance HasTextElementWithStrategy 'Default syntax where
  isTextElementWithStrategy _ _ = False

instance CustomHasTextElement syntax => HasTextElementWithStrategy 'Custom syntax where
  isTextElementWithStrategy _ = customIsTextElement
