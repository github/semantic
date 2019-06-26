{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Hedgehog
import Control.Monad

import Data.Core
import Language.Python.Core
import qualified TreeSitter.Python.AST as Py

prog1 :: Py.Module
prog1 = Py.Module $ Just [ Right . Py.PassStatementSimpleStatement . Py.PassStatement $ "pass"
                         ]

test_compile_prog1 :: Property
test_compile_prog1 = property $
  compile prog1 === Just Unit

main :: IO ()
main = void $ checkParallel $$(discoverPrefix "test_")
