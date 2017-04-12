module Data.Syntax.Type where

import Prologue

data Annotation a = Annotation { annotationSubject :: !a, annotationType :: !a }
  deriving (Eq, Show)

newtype Product a = Product { productElements :: [a] }
  deriving (Eq, Show)
