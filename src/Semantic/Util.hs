{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Semantic.Util where

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Analysis.Abstract.Evaluating
import           Control.Abstract
import           Control.Monad.Effect.Trace (runPrintingTrace)
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Value
import           Data.Abstract.Type
import           Data.Blob
import           Data.Project
import           Data.Functor.Foldable
import qualified Data.Language as Language
import           Data.Sum (weaken)
import           Data.Term
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Prologue hiding (weaken)
import           Semantic.Graph
import           Semantic.IO as IO
import           Semantic.Task
import           Text.Show (showListWith)
import           Text.Show.Pretty (ppShow)

justEvaluating
  = runM
  . evaluating
  . runPrintingTrace
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runEnvironmentError
  . runEvalError
  . runResolutionError
  . runAddressError
  . runTermEvaluator @_ @Precise @(Value Precise (UtilEff _))
  . runValueError

newtype UtilEff address a = UtilEff
  { runUtilEff :: Eff '[ LoopControl address
                       , Exc (Return address)
                       , Env address
                       , Allocator address (Value address (UtilEff address))
                       , Reader ModuleInfo
                       , Modules address (Value address (UtilEff address))
                       , Reader Span
                       , Reader PackageInfo
                       , Resumable (ValueError address (UtilEff address))
                       , Resumable (AddressError address (Value address (UtilEff address)))
                       , Resumable ResolutionError
                       , Resumable EvalError
                       , Resumable (EnvironmentError address)
                       , Resumable (Unspecialized (Value address (UtilEff address)))
                       , Resumable (LoadError address (Value address (UtilEff address)))
                       , Trace
                       , Fresh
                       , State (Heap address Latest (Value address (UtilEff address)))
                       , State (ModuleTable (Maybe (Environment address, address)))
                       , Lift IO
                       ] a
  }

checking
  = runM @_ @IO
  . evaluating
  . runPrintingTrace
  . runTermEvaluator @_ @Monovariant @Type
  . caching @[]
  . providingLiveSet
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runResolutionError
  . runEnvironmentError
  . runEvalError
  . runAddressError
  . runTypeError

evalGoProject path         = justEvaluating =<< evaluateProject (Proxy :: Proxy 'Language.Go)         goParser         Language.Go         path
evalRubyProject path       = justEvaluating =<< evaluateProject (Proxy :: Proxy 'Language.Ruby)       rubyParser       Language.Ruby       path
evalPHPProject path        = justEvaluating =<< evaluateProject (Proxy :: Proxy 'Language.PHP)        phpParser        Language.PHP        path
evalPythonProject path     = justEvaluating =<< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser     Language.Python     path
evalJavaScriptProject path = justEvaluating =<< evaluateProject (Proxy :: Proxy 'Language.JavaScript) typescriptParser Language.JavaScript path
evalTypeScriptProject path = justEvaluating =<< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser Language.TypeScript path

typecheckGoFile path = checking =<< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Go) goParser Language.Go path

-- Evaluate a project, starting at a single entrypoint.
evaluateProject proxy parser lang path = evaluatePackageWith proxy id withTermSpans . fmap quieterm <$> runTask (readProject Nothing path lang [] >>= parsePackage parser)
evaluateProjectWithCaching proxy parser lang path = evaluatePackageWith proxy convergingModules (withTermSpans . cachingTerms) . fmap quieterm <$> runTask (readProject Nothing path lang [] >>= parsePackage parser)


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file


mergeExcs :: Either (SomeExc (Sum excs)) (Either (SomeExc exc) result) -> Either (SomeExc (Sum (exc ': excs))) result
mergeExcs = either (\ (SomeExc sum) -> Left (SomeExc (weaken sum))) (either (\ (SomeExc exc) -> Left (SomeExc (inject exc))) Right)

reassociate :: Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) (Either (SomeExc exc4) (Either (SomeExc exc5) (Either (SomeExc exc6) (Either (SomeExc exc7) result)))))) -> Either (SomeExc (Sum '[exc7, exc6, exc5, exc4, exc3, exc2, exc1])) result
reassociate = mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . Right


newtype Quieterm syntax ann = Quieterm { unQuieterm :: TermF syntax ann (Quieterm syntax ann) }
  deriving (Declarations, FreeVariables)

type instance Base (Quieterm syntax ann) = TermF syntax ann
instance Functor syntax => Recursive   (Quieterm syntax ann) where project = unQuieterm
instance Functor syntax => Corecursive (Quieterm syntax ann) where embed   =   Quieterm

instance Eq1 syntax => Eq1 (Quieterm syntax) where
  liftEq eqA = go where go t1 t2 = liftEq2 eqA go (unQuieterm t1) (unQuieterm t2)

instance (Eq1 syntax, Eq ann) => Eq (Quieterm syntax ann) where
  (==) = eq1

instance Ord1 syntax => Ord1 (Quieterm syntax) where
  liftCompare comp = go where go t1 t2 = liftCompare2 comp go (unQuieterm t1) (unQuieterm t2)

instance (Ord1 syntax, Ord ann) => Ord (Quieterm syntax ann) where
  compare = compare1

instance Show1 syntax => Show1 (Quieterm syntax) where
  liftShowsPrec _ _ = go where go d = liftShowsPrec go (showListWith (go 0)) d . termFOut . unQuieterm

instance Show1 syntax => Show (Quieterm syntax ann) where
  showsPrec = liftShowsPrec (const (const id)) (const id)

quieterm :: (Recursive term, Base term ~ TermF syntax ann) => term -> Quieterm syntax ann
quieterm = cata Quieterm


prettyShow :: Show a => a -> IO ()
prettyShow = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow
