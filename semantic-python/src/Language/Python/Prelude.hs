{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DerivingVia, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeApplications, TypeOperators, UndecidableInstances #-}

module Language.Python.Prelude where

import Data.File
import Data.Core
import Control.Effect
import Language.Preface

prelude :: (Member Preface sig, Carrier sig m) => m ()
prelude = do
  metaclass "type"
  klass "object" $ do
    inherit "type"
    method "__new__" ["self"] $ do
      def "instance" frame
      open "instance" $ inherit "self"
      call "instance" "__init__" ["instance"]
    method "__init__" ["self"] $ var "self"

  klass "NoneType" $ inherit "object"

  def "None" $ new "NoneType"

  klass "bool" $ do
    inherit "object"
    method "__bool__" ["self"] $ var "self"

  def "True" $ new "bool"
  def "False" $ new "bool"

  open "object" $ do
    method "_bool__" ["self"] $ var "False"

python :: File Core
python = undefined
