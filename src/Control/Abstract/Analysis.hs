{-# LANGUAGE FunctionalDependencies #-}
module Control.Abstract.Analysis
( AnalyzeTerm(..)
, AnalyzeModule(..)
, module X
) where

import Control.Abstract.Addressable as X
import Control.Abstract.Evaluator as X
import Control.Abstract.Value as X
import Control.Effect as X
import Control.Monad.Effect.Fail as X
import Control.Monad.Effect.Fresh as X
import Control.Monad.Effect.Internal as X (Eff, relay)
import Control.Monad.Effect.NonDet as X
import Control.Monad.Effect.Reader as X
import Control.Monad.Effect.State as X
import Control.Monad.Effect.Resumable as X
import Data.Abstract.Module
import Prologue

-- | A context enabling the analysis of terms, possibly providing effects to underlying analyses.
class (Evaluator location term value m, Monad (m outer)) => AnalyzeTerm location term value inner outer m | m inner -> outer, m outer -> inner where
  -- | Analyze a term using the semantics of the current analysis.
  analyzeTerm :: Effectful outside
              => (Base term (Subterm term (outside inner value)) -> m inner value)
              -> (Base term (Subterm term (outside outer value)) -> m outer value)

-- | A context enabling the analysis of modules, possibly providing effects to underlying analyses.
class (Evaluator location term value m, Monad (m outer)) => AnalyzeModule location term value inner outer m | m inner -> outer, m outer -> inner where
  -- | Analyze a module using the semantics of the current analysis. This should generally only be called by 'evaluateModule' and by definitions of 'analyzeModule' in instances for composite analyses.
  analyzeModule :: Effectful outside
                => (Module (Subterm term (outside inner value)) -> m inner value)
                -> (Module (Subterm term (outside outer value)) -> m outer value)
