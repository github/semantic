{-# LANGUAGE AllowAmbiguousTypes, DataKinds, TypeFamilies #-}

module Language.Preluded
    ( Preluded (..)
    ) where

import Control.Abstract
import GHC.Stack
import GHC.TypeLits
import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby
import qualified Language.TypeScript.Assignment as TypeScript

class Preluded syntax where
  type PreludePath syntax :: Symbol
  definePrelude :: ( AbstractValue address value effects
                   , HasCallStack
                   , Member (Allocator address value) effects
                   , Member (Env address) effects
                   , Member Fresh effects
                   , Member (Reader ModuleInfo) effects
                   , Member (Reader Span) effects
                   , Member (Resumable (EnvironmentError address)) effects
                   , Member Trace effects
                   )
                => Evaluator address value effects ()

instance Preluded Ruby.Term where
  type PreludePath Ruby.Term = "preludes/ruby.rb"

  definePrelude = pure ()

instance Preluded Python.Term where
  type PreludePath Python.Term = "preludes/python.py"

  definePrelude = pure ()

instance Preluded TypeScript.Term where
  type PreludePath TypeScript.Term = "preludes/javascript.js"

  definePrelude = pure ()
