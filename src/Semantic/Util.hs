{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Evaluating as X
import           Control.Abstract.Evaluator
import           Control.Abstract.TermEvaluator
import           Control.Monad.Effect.Trace (runPrintingTrace)
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Value
import           Data.Abstract.Type
import           Data.Blob
import           Data.File
import           Data.Functor.Foldable
import qualified Data.Language as Language
import           Data.Term
import qualified GHC.TypeLits as TypeLevel
import           Language.Preluded
import           Parsing.Parser
import           Prologue
import           Semantic.Graph
import           Semantic.IO as IO
import           Semantic.Task
import           Text.Show (showListWith)

import qualified Language.Python.Assignment as Python
import qualified Language.Ruby.Assignment as Ruby

justEvaluating
  = runM
  . fmap (first reassociate)
  . evaluating
  . runPrintingTrace
  . runLoadError
  . runValueError
  . runUnspecialized
  . runResolutionError
  . runEnvironmentError
  . runEvalError
  . runAddressError
  . runTermEvaluator @_ @Precise

evaluatingWithHoles
  = runM
  . evaluating
  . runPrintingTrace
  . resumingLoadError
  . resumingUnspecialized
  . resumingValueError
  . resumingEnvironmentError
  . resumingEvalError
  . resumingResolutionError
  . resumingAddressError
  . runTermEvaluator @_ @Precise

checking
  = runM @_ @IO
  . evaluating
  . runPrintingTrace
  . runTermEvaluator @_ @Monovariant @Type
  . caching @[]
  . providingLiveSet
  . runLoadError
  . runUnspecialized
  . runResolutionError
  . runEnvironmentError
  . runEvalError
  . runAddressError
  . runTypeError

evalGoProject path = justEvaluating =<< evaluateProject goParser Language.Go Nothing path
evalRubyProject path = justEvaluating =<< evaluateProject rubyParser Language.Ruby rubyPrelude path
evalPHPProject path = justEvaluating =<< evaluateProject phpParser Language.PHP Nothing path
evalPythonProject path = justEvaluating =<< evaluateProject pythonParser Language.Python pythonPrelude path
evalTypeScriptProjectQuietly path = evaluatingWithHoles =<< evaluateProject typescriptParser Language.TypeScript Nothing path
evalTypeScriptProject path = justEvaluating =<< evaluateProject typescriptParser Language.TypeScript Nothing path

typecheckGoFile path = checking =<< evaluateProjectWithCaching goParser Language.Go Nothing path

rubyPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Ruby.Term))) (Just Language.Ruby)
pythonPrelude = Just $ File (TypeLevel.symbolVal (Proxy :: Proxy (PreludePath Python.Term))) (Just Language.Python)

-- Evaluate a project, starting at a single entrypoint.
evaluateProject parser lang prelude path = evaluatePackageWith id withTermSpans <$> runTask (readProject Nothing path lang [] >>= parsePackage parser prelude)
evaluateProjectWithCaching parser lang prelude path = evaluatePackageWith convergingModules (withTermSpans . cachingTerms) <$> runTask (readProject Nothing path lang [] >>= parsePackage parser prelude)


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file


injectConst :: a -> SomeExc (Sum '[Const a])
injectConst = SomeExc . injectSum . Const

mergeExcs :: Either (SomeExc (Sum excs)) (Either (SomeExc exc) result) -> Either (SomeExc (Sum (exc ': excs))) result
mergeExcs = either (\ (SomeExc sum) -> Left (SomeExc (weakenSum sum))) (either (\ (SomeExc exc) -> Left (SomeExc (injectSum exc))) Right)

reassociate = mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . first injectConst
reassociateTypes = mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . first injectConst


newtype Quieterm syntax ann = Quieterm { unQuieterm :: TermF syntax ann (Quieterm syntax ann) }
  deriving (Declarations, FreeVariables)

type instance Base (Quieterm syntax ann) = TermF syntax ann
instance Functor syntax => Recursive   (Quieterm syntax ann) where project = unQuieterm
instance Functor syntax => Corecursive (Quieterm syntax ann) where embed   =   Quieterm

instance Eq1 syntax => Eq1 (Quieterm syntax) where
  liftEq eqA = go where go t1 t2 = liftEq2 eqA go (unQuieterm t1) (unQuieterm t2)

instance (Eq1 syntax, Eq ann) => Eq (Quieterm syntax ann) where
  (==) = eq1

instance Show1 syntax => Show1 (Quieterm syntax) where
  liftShowsPrec _ _ = go where go d = liftShowsPrec go (showListWith (go 0)) d . termFOut . unQuieterm

instance Show1 syntax => Show (Quieterm syntax ann) where
  showsPrec = liftShowsPrec (const (const id)) (const id)

quieterm :: (Recursive term, Base term ~ TermF syntax ann) => term -> Quieterm syntax ann
quieterm = cata Quieterm
