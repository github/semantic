module Control.Abstract.Evaluator.Spec where

import Analysis.Abstract.Evaluating (evaluating)
import Control.Abstract
import Control.Monad.Effect (runM)
import Data.Abstract.Module
import qualified Data.Abstract.Number as Number
import Data.Abstract.Package
import qualified Data.Abstract.Value as Value
import Data.Algebra
import SpecHelpers hiding (Term)

spec :: Spec
spec = parallel $ do
  it "constructs integers" $ do
    (expected, _) <- evaluate (integer 123)
    expected `shouldBe` Right (Right (Right (Value.injValue (Value.Integer (Number.Integer 123)))))
  pure ()

evaluate
  = runM
  . lower
  . evaluating
  . runReader (PackageInfo (name "test") Nothing)
  . runReader (ModuleInfo "test/Control/Abstract/Evaluator/Spec.hs")
  . Value.runValueError
  . runAddressError
  . constrainTerm
  . runValue
runValue = runEvalClosure (runValue . runTerm) . runReturn . runLoopControl

constrainTerm :: Evaluator location Term value effects a -> Evaluator location Term value effects a
constrainTerm = id

term :: TermEvaluator Value -> Subterm Term (TermEvaluator Value)
term eval = Subterm (Term eval) eval

type TermEffects
  = '[ LoopControl Value
     , Return Value
     , EvalClosure Term Value
     , Resumable (AddressError Precise Value)
     , Resumable (Value.ValueError Precise Value)
     , Reader ModuleInfo
     , Reader PackageInfo
     , Fail
     , Fresh
     , Reader (Environment Precise Value)
     , State (Environment Precise Value)
     , State (Heap Precise Value)
     , State (ModuleTable (Environment Precise Value, Value))
     , State (Exports Precise Value)
     , State (JumpTable Term)
     , IO
     ]

type TermEvaluator = Evaluator Precise Term Value TermEffects

type Value = Value.Value Precise
newtype Term = Term { runTerm :: TermEvaluator Value }

instance Show Term where showsPrec d _ = showParen (d > 10) $ showString "Term _"
