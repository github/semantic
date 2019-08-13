{-# LANGUAGE FlexibleInstances, TypeApplications #-}

module Main (main) where

import           Control.Effect
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import qualified Data.ByteString as ByteString
import           Data.Core
import           Data.Foldable
import           Data.List (sort, isInfixOf)
import           Data.Name
import           Data.Term
import           GHC.Stack
import qualified Language.Python.Core as Py
import           System.Directory
import           System.FilePath
import qualified TreeSitter.Python as TSP
import qualified TreeSitter.Python.AST as TSP
import qualified TreeSitter.Unmarshal as TS

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

instance MonadFail (Either String) where fail = Left

assertTranslationSucceeds :: HasCallStack => FilePath -> HUnit.Assertion
assertTranslationSucceeds fp = withFrozenCallStack $ do
  let shouldFail = "fails" `isInfixOf` fp || "disabled" `isInfixOf` fp
  parsed <- ByteString.readFile ("semantic-python/test/fixtures" </> fp) >>= TS.parseByteString TSP.tree_sitter_python
  case fmap (Py.compile @TSP.Module @_ @(Term Core)) parsed of
    Right (Left err) -> unless shouldFail (HUnit.assertFailure ("Compilation failed: " <> err))
    Left err         -> HUnit.assertFailure ("Parsing failed: " <> err)
    _                -> pure ()

milestoneFixtures :: Tasty.TestTree
milestoneFixtures = HUnit.testCaseSteps "Bootstrapping" $ \step -> do
  files <- liftIO (listDirectory "semantic-python/test/fixtures")
  let firstGroup = sort $ filter ((== '1') . head) files
  for_ firstGroup $ \file -> do
    step file
    assertTranslationSucceeds file


tests :: Tasty.TestTree
tests = Tasty.testGroup "Fixtures"
  [ milestoneFixtures
  ]

main :: IO ()
main = Tasty.defaultMain tests
