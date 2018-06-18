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
import Prologue

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

  definePrelude = do
    define "puts" (lambda (\ v -> do
      print <- variable "__semantic_print" >>= deref
      void $ call print [variable v]
      box unit))

    defineClass "Object" [] $ do
      define "inspect" (lambda (const (box (string "<object>"))))


instance Preluded Python.Term where
  type PreludePath Python.Term = "preludes/python.py"

  definePrelude =
    define "print" (lambda (\ v -> do
      print <- variable "__semantic_print" >>= deref
      void $ call print [variable v]
      box unit))

instance Preluded TypeScript.Term where
  type PreludePath TypeScript.Term = "preludes/javascript.js"

  -- FIXME: define console.log using __semantic_print
  definePrelude = pure ()
