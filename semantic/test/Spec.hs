{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Main (allTests, legacySpecs, main, tests) where

import qualified Data.Graph.Spec
import qualified Data.Language.Spec
import qualified Data.Semigroup.App.Spec
import qualified Integration.Spec
import qualified Semantic.CLI.Spec
import           Semantic.Config (defaultOptions, optionsLogLevel)
import qualified Semantic.IO.Spec
import qualified Semantic.Spec
import qualified Semantic.Stat.Spec
import           Semantic.Task (TaskSession (..), withOptions)
import qualified System.Path.Fixture as Fixture
import qualified Tags.Spec
import           Test.Hspec
import           Test.Tasty as Tasty
import           Test.Tasty.Hspec as Tasty

tests :: (?session :: TaskSession, Fixture.HasFixture) => [TestTree]
tests =
  [ Data.Language.Spec.testTree
  , Data.Semigroup.App.Spec.testTree
  , Integration.Spec.testTree
  , Semantic.CLI.Spec.testTree
  , Semantic.Stat.Spec.testTree
  ]

-- We can't bring this out of the IO monad until we divest
-- from hspec, since testSpec operates in IO.
allTests :: (?session :: TaskSession, Fixture.HasFixture) => IO TestTree
allTests = do
  asTastySpecs <- Tasty.testSpecs legacySpecs
  let allSpecs = tests <> asTastySpecs
  pure (testGroup "semantic" allSpecs)

-- If you're writing new test modules, please don't add to this
-- stanza: it is only there to prevent massive rewrites, and is
-- converted into a Tasty TestTree in 'main'. (Quoth the tasty-hspec
-- documentation: "hspec and tasty serve similar purposes; consider
-- using one or the other.") Instead, create a new TestTree value
-- in your spec module and add it to the above 'tests' list.
legacySpecs :: Fixture.HasFixture => Spec
legacySpecs = parallel $ do
  describe "Data.Graph" Data.Graph.Spec.spec
  describe "Tags.Spec" Tags.Spec.spec
  describe "Semantic" Semantic.Spec.spec
  describe "Semantic.IO" Semantic.IO.Spec.spec


main :: IO ()
main = do
  runfiles <- Fixture.create
  let ?runfiles = runfiles
      ?project = "semantic"

  withOptions defaultOptions { optionsLogLevel = Nothing } $ \ config logger statter ->
    let ?session = TaskSession config "-" False logger statter

    in allTests >>= defaultMain
