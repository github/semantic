{-# LANGUAGE DataKinds, FlexibleContexts, PackageImports, PartialTypeSignatures, TypeApplications, TypeFamilies #-}

module Main where

import           Algebra.Graph
import           Control.Monad
import           Data.Abstract.Evaluatable
import           Data.Abstract.FreeVariables
import           Data.Blob
import           Data.Blob.IO (readBlobFromFile')
import           Data.Bifunctor
import           Data.Functor.Classes
import           "semantic" Data.Graph (Graph (..), topologicalSort)
import           Data.Graph.ControlFlowVertex
import qualified Data.Language as Language
import           Data.Project
import           Data.Proxy
import           Data.Term
import           Gauge.Main
import           Parsing.Parser
import           Semantic.Config (defaultOptions)
import           Semantic.Graph
import           Semantic.Task (SomeException, TaskSession (..), runTask, withOptions)
import           Semantic.Util hiding (evalPythonProject, evalRubyProject, evaluateProject)
import           Source.Loc
import qualified System.Path as Path
import           System.Path ((</>))

-- Duplicating this stuff from Util to shut off the logging

callGraphProject' :: ( Language.SLanguage lang
                     , Ord1 syntax
                     , Declarations1 syntax
                     , Evaluatable syntax
                     , FreeVariables1 syntax
                     , AccessControls1 syntax
                     , HasPrelude lang
                     , Functor syntax
                     , VertexDeclarationWithStrategy (VertexDeclarationStrategy syntax) syntax syntax
                     )
                  => TaskSession
                  -> Proxy lang
                  -> Parser (Term syntax Loc)
                  -> Path.RelFile
                  -> IO (Either String (Data.Graph.Graph ControlFlowVertex))
callGraphProject' session proxy parser path = fmap (first show) . runTask session $ do
  blob <- readBlobFromFile' (fileForRelPath path)
  package <- fmap snd <$> parsePackage parser (Project (Path.toString (Path.takeDirectory path)) [blob] (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  runCallGraph proxy False modules package

callGraphProject proxy parser paths = withOptions defaultOptions $ \ config logger statter ->
  callGraphProject' (TaskSession config "" False logger statter) proxy parser paths

evalRubyProject       = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Ruby)       rubyParser
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser

evaluateProject proxy parser path = withOptions defaultOptions $ \ config logger statter ->
  evaluateProject' (TaskSession config "" False logger statter) proxy parser [Path.toString path]

pyEval :: Path.RelFile -> Benchmarkable
pyEval p = nfIO $ evalPythonProject (Path.relDir "bench/bench-fixtures/python" </> p)

rbEval :: Path.RelFile -> Benchmarkable
rbEval p = nfIO $ evalRubyProject (Path.relDir "bench/bench-fixtures/python" </> p)

pyCall :: Path.RelFile -> Benchmarkable
pyCall p = nfIO $ callGraphProject (Proxy @'Language.Python) pythonParser (Path.relDir "bench/bench-fixtures/python/" </> p)

rbCall :: Path.RelFile -> Benchmarkable
rbCall p = nfIO $ callGraphProject (Proxy @'Language.Ruby) rubyParser $ (Path.relDir "bench/bench-fixtures/ruby" </> p)

main :: IO ()
main = defaultMain
  [ bgroup "python" [ bench "assignment" . pyEval $ Path.relFile "simple-assignment.py"
                    , bench "function def" . pyEval $ Path.relFile "function-definition.py"
                    , bench "if + function calls" . pyCall . Path.relFile $ "if-statement-functions.py"
                    , bench "call graph" $ pyCall . Path.relFile $ "if-statement-functions.py"
                    ]
  , bgroup "ruby" [ bench "assignment" . rbEval $ Path.relFile "simple-assignment.rb"
                  , bench "function def" . rbEval . Path.relFile $ "function-definition.rb"
                  , bench "if + function calls" . rbCall $ Path.relFile "if-statement-functions.rb"
                  , bench "call graph" $ rbCall $ Path.relFile "if-statement-functions.rb"
                  ]
  ]
