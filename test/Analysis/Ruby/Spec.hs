module Analysis.Ruby.Spec (spec) where

import Data.Abstract.Environment as Env
import Data.Abstract.Evaluatable
import Data.Abstract.Value as Value
import Data.Abstract.Number as Number
import Data.AST
import Control.Monad.Effect (SomeExc(..))
import Data.List.NonEmpty (NonEmpty(..))
import Data.Sum
import qualified Language.Ruby.Assignment as Ruby
import qualified Data.Language as Language

import SpecHelpers


spec :: Spec
spec = parallel $ do
  describe "Ruby" $ do
    pure ()
    -- it "evaluates require_relative" $ do
    --   ((res@(~(Right [(_, env)])), _), _) <- evaluate ["main.rb", "foo.rb"]
    --   map fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 1)]
    --   Env.names env `shouldContain` ["foo"]
    --
    -- it "evaluates load" $ do
    --   ((res@(~(Right [(_, env)])), _), _) <- evaluate ["load.rb", "foo.rb"]
    --   map fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 1)]
    --   Env.names env `shouldContain` ["foo"]
    --
    -- it "evaluates load with wrapper" $ do
    --   ((res, _), _) <- evaluate ["load-wrap.rb", "foo.rb"]
    --   res `shouldBe` Left (SomeExc (inject @(EnvironmentError Precise) (FreeVariable "foo")))
    --
    -- it "evaluates subclass" $ do
    --   ((res@(~(Right [(_, env)])), heap), _) <- evaluate ["subclass.rb"]
    --   map fst <$> res `shouldBe` Right [String "\"<bar>\""]
    --   Env.names env `shouldContain` [ "Bar", "Foo" ]
    --
    --   (derefQName heap ("Bar" :| []) env >>= deNamespace) `shouldBe` Just ("Bar",  ["baz", "foo", "inspect"])
    --
    -- it "evaluates modules" $ do
    --   ((res@(~(Right [(_, env)])), _), _) <- evaluate ["modules.rb"]
    --   map fst <$> res `shouldBe` Right [String "\"<hello>\""]
    --   Env.names env `shouldContain` [ "Bar" ]
    --
    -- it "handles break correctly" $ do
    --   ((res, _), _) <- evaluate ["break.rb"]
    --   fmap fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 3)]
    --
    -- it "handles break correctly" $ do
    --   ((res, _), _) <- evaluate ["next.rb"]
    --   fmap fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 8)]
    --
    -- it "calls functions with arguments" $ do
    --   ((res, _), _) <- evaluate ["call.rb"]
    --   fmap fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 579)]
    --
    -- it "evaluates early return statements" $ do
    --   ((res, _), _) <- evaluate ["early-return.rb"]
    --   fmap fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 123)]
    --
    -- it "has prelude" $ do
    --   ((res, _), _) <- evaluate ["preluded.rb"]
    --   fmap fst <$> res `shouldBe` Right [String "\"<foo>\""]
    --
    -- it "evaluates __LINE__" $ do
    --   ((res, _), _) <- evaluate ["line.rb"]
    --   fmap fst <$> res `shouldBe` Right [Value.Integer (Number.Integer 4)]
    --
    -- it "resolves builtins used in the prelude" $ do
    --   ((res, _), traces) <- evaluate ["puts.rb"]
    --   fmap fst <$> res `shouldBe` Right [Unit]
    --   traces `shouldContain` [ "\"hello\"" ]

  where
    ns n = Just . Latest . Last . Just . Namespace n
    fixtures = "test/fixtures/ruby/analysis/"
    evaluate = evalRubyProject . map (fixtures <>)
    evalRubyProject = testEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Ruby) rubyParser Language.Ruby
