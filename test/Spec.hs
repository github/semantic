module Main where

import qualified Analysis.Go.Spec
import qualified Analysis.PHP.Spec
import qualified Analysis.Python.Spec
import qualified Analysis.Ruby.Spec
import qualified Analysis.TypeScript.Spec
import qualified Assigning.Assignment.Spec
import qualified Control.Abstract.Evaluator.Spec
import qualified Control.Rewriting.Spec
import qualified Data.Diff.Spec
import qualified Data.Abstract.Name.Spec
import qualified Data.Abstract.Path.Spec
import qualified Data.Functor.Classes.Generic.Spec
import qualified Data.Graph.Spec
import qualified Data.Range.Spec
import qualified Data.Scientific.Spec
import qualified Data.Semigroup.App.Spec
import qualified Data.Source.Spec
import qualified Data.Term.Spec
import qualified Diffing.Algorithm.RWS.Spec
import qualified Diffing.Algorithm.SES.Spec
import qualified Diffing.Interpreter.Spec
import qualified Graphing.Calls.Spec
import qualified Integration.Spec
import qualified Matching.Go.Spec
import qualified Matching.Python.Spec
import qualified Numeric.Spec
import qualified Rendering.TOC.Spec
import qualified Reprinting.Spec
import qualified Tags.Spec
import qualified Semantic.Spec
import qualified Semantic.CLI.Spec
import qualified Semantic.IO.Spec
import qualified Semantic.Stat.Spec
import Semantic.Config (defaultOptions)
import Semantic.Task (withOptions)
import Semantic.Util (TaskConfig(..))
import qualified Proto3.Roundtrip
import Test.Hspec

main :: IO ()
main = do
  withOptions defaultOptions $ \ config logger statter -> hspec $ do
    let args = TaskConfig config logger statter
    describe "Semantic.Stat" Semantic.Stat.Spec.spec
    parallel $ do
      describe "Analysis.Go" (Analysis.Go.Spec.spec args)
      describe "Analysis.PHP" (Analysis.PHP.Spec.spec args)
      describe "Analysis.Python" (Analysis.Python.Spec.spec args)
      describe "Analysis.Ruby" (Analysis.Ruby.Spec.spec args)
      describe "Analysis.TypeScript" (Analysis.TypeScript.Spec.spec args)
      describe "Assigning.Assignment" Assigning.Assignment.Spec.spec
      describe "Control.Abstract.Evaluator" Control.Abstract.Evaluator.Spec.spec
      describe "Control.Rewriting.Spec" Control.Rewriting.Spec.spec
      describe "Data.Diff" Data.Diff.Spec.spec
      describe "Data.Graph" Data.Graph.Spec.spec
      describe "Data.Abstract.Path" Data.Abstract.Path.Spec.spec
      describe "Data.Abstract.Name" Data.Abstract.Name.Spec.spec
      describe "Data.Functor.Classes.Generic" Data.Functor.Classes.Generic.Spec.spec
      describe "Data.Range" Data.Range.Spec.spec
      describe "Data.Scientific" Data.Scientific.Spec.spec
      describe "Data.Semigroup.App" Data.Semigroup.App.Spec.spec
      describe "Data.Source" Data.Source.Spec.spec
      describe "Data.Term" Data.Term.Spec.spec
      describe "Diffing.Algorithm.RWS" Diffing.Algorithm.RWS.Spec.spec
      describe "Diffing.Algorithm.SES" Diffing.Algorithm.SES.Spec.spec
      describe "Diffing.Interpreter" Diffing.Interpreter.Spec.spec
      describe "Graphing.Calls" Graphing.Calls.Spec.spec
      describe "Matching.Go" Matching.Go.Spec.spec
      describe "Matching.Python" Matching.Python.Spec.spec
      describe "Numeric" Numeric.Spec.spec
      describe "Rendering.TOC" Rendering.TOC.Spec.spec
      describe "Reprinting.Spec" Reprinting.Spec.spec
      describe "Tags.Spec" Tags.Spec.spec
      describe "Semantic" Semantic.Spec.spec
      describe "Semantic.CLI" Semantic.CLI.Spec.spec
      describe "Semantic.IO" Semantic.IO.Spec.spec
      describe "Integration" (Integration.Spec.spec args)
      describe "Protobuf roundtripping" Proto3.Roundtrip.spec
