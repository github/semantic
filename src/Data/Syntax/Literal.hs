{-# LANGUAGE DataKinds #-}
module Data.Syntax.Literal where

import Data.Functor.Union
import Prologue


-- Strings, symbols

newtype StringLiteral a = StringLiteral { stringElements :: [Union '[InterpolationElement, TextElement] a] } -- may also wish to include escapes
  deriving (Eq, Show)

-- | An interpolation element within a string literal.
newtype InterpolationElement a = InterpolationElement { interpolationBody :: a }
  deriving (Eq, Show)

-- | A sequence of textual contents within a string literal.
newtype TextElement a = TextElement { textElementContent :: ByteString }
  deriving (Eq, Show)
