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

assertCompilesTo :: (MonadTest m, Show a, Compile a) => [Core] -> a -> m ()
assertCompilesTo valids a = do
  annotateShow a
  assert (compile a `elem` fmap Just valids)

prop_compiles_booleans = property $
  forAll PyGen.boolean >>= assertCompilesTo [Bool True, Bool False]

prop_assert_compile_prog1 = property $
  compile prog1 === Just Unit

main :: IO ()
main = void $ checkParallel $$(discover)
