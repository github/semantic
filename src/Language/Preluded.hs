module Language.Preluded
    ( Preluded (..)
    , export
    ) where

import GHC.TypeLits
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby

class Preluded syntax where
  type PreludePath syntax :: Symbol

instance Preluded Ruby.Term where
  type PreludePath Ruby.Term = "preludes/ruby.rb"

instance Preluded Python.Term where
  type PreludePath Python.Term = "preludes/python.py"
