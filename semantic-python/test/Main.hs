{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main where

import Hedgehog
import Control.Monad

import Data.Core
import Language.Python.Core
import qualified Language.Python.Generators as PyGen
import qualified TreeSitter.Python.AST as Py

prog1 :: Py.Module
prog1 = Py.Module $ Just [ Right . Py.PassStatementSimpleStatement . Py.PassStatement $ "pass"
                         ]

prop_compiles_booleans = property $ do
  b <- forAll PyGen.boolean
  assert (compile b `elem` [Just $ Bool True, Just $ Bool False])

prop_assert_compile_prog1 = property $
  compile prog1 === Just Unit

main :: IO ()
main = void $ checkParallel $$(discover)
