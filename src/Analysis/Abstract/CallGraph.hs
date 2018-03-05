{-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TypeApplications, TypeFamilies, UndecidableInstances #-}
module Analysis.Abstract.CallGraph where

import qualified Algebra.Graph as G
import Algebra.Graph.Class
import Algebra.Graph.Export.Dot
import Control.Abstract.Evaluator
import Control.Effect
import Control.Monad.Effect.Fail
import Control.Monad.Effect.NonDetEff
import Control.Monad.Effect.Reader
import Control.Monad.Effect.State
import Data.Abstract.Address
import Data.Abstract.Evaluatable
import Data.Abstract.Linker
import Data.Abstract.Value
import Data.Proxy
import qualified Data.Syntax.Declaration as Declaration
import Data.Term
import Prologue hiding (empty)

type CallGraphEffects term
  = '[ Fail
     , NonDetEff
     , State  (StoreFor CallGraph)
     , State  (EnvironmentFor CallGraph)
     , Reader (EnvironmentFor CallGraph)
     , Reader (Linker term)
     , State  (Linker CallGraph)
     ]

newtype CallGraph = CallGraph { unCallGraph :: G.Graph Name }
  deriving (Eq, Graph, Show)

renderCallGraph :: CallGraph -> ByteString
renderCallGraph = export (defaultStyle id) . unCallGraph


newtype CallGraphAnalysis term a = CallGraphAnalysis { runCallGraphAnalysis :: Evaluator (CallGraphEffects term) term CallGraph a }
  deriving (Alternative, Applicative, Functor, Monad, MonadFail)

deriving instance MonadEvaluator term CallGraph (CallGraphAnalysis term)

evaluateCallGraph :: forall term
                  .  ( Evaluatable (Base term)
                     , Foldable (Base term)
                     , FreeVariables term
                     , IsDeclaration (Base term)
                     , MonadAddressable (LocationFor CallGraph) CallGraph (CallGraphAnalysis term)
                     , MonadValue term CallGraph (CallGraphAnalysis term)
                     , Ord (LocationFor CallGraph)
                     , Ord term
                     , Recursive term
                     , Semigroup (Cell (LocationFor CallGraph) CallGraph)
                     )
                  => term
                  -> Final (CallGraphEffects term) CallGraph
evaluateCallGraph = run @(CallGraphEffects term) . runEvaluator . runCallGraphAnalysis . evaluateTerm

instance MonadValue term CallGraph (CallGraphAnalysis term) where
  unit = pure empty
  integer _ = pure empty
  boolean _ = pure empty
  string _ = pure empty

  ifthenelse _ then' else' = overlay <$> then' <*> else'

  abstract _ = subtermValue

  apply operator arguments = foldr overlay operator <$> traverse subtermValue arguments

type instance LocationFor CallGraph = Monovariant


instance ( Evaluatable (Base term)
         , FreeVariables term
         , IsDeclaration (Base term)
         , MonadAddressable (LocationFor CallGraph) CallGraph (CallGraphAnalysis term)
         , MonadValue term CallGraph (CallGraphAnalysis term)
         , Ord term
         , Recursive term
         , Semigroup (Cell (LocationFor CallGraph) CallGraph)
         )
         => MonadAnalysis term CallGraph (CallGraphAnalysis term) where
  evaluateTerm = foldSubterms (\ term ->
    connectDeclaration (subterm <$> term) <$> eval term)


class IsDeclaration syntax where
  connectDeclaration :: FreeVariables term => syntax term -> CallGraph -> CallGraph

instance (IsDeclarationStrategy syntax ~ strategy, IsDeclarationWithStrategy strategy syntax) => IsDeclaration syntax where
  connectDeclaration graph = connectDeclarationWithStrategy (Proxy :: Proxy strategy) graph

class CustomIsDeclaration syntax where
  customConnectDeclaration :: FreeVariables term => syntax term -> CallGraph -> CallGraph

instance CustomIsDeclaration Declaration.Function where
  customConnectDeclaration Declaration.Function{..} = flip (foldr (connect . vertex)) (freeVariables functionName)

instance CustomIsDeclaration Declaration.Method where
  customConnectDeclaration Declaration.Method{..} = flip (foldr (connect . vertex)) (freeVariables methodName)

instance Apply IsDeclaration syntaxes => CustomIsDeclaration (Union syntaxes) where
  customConnectDeclaration = Prologue.apply (Proxy :: Proxy IsDeclaration) connectDeclaration

instance IsDeclaration syntax => CustomIsDeclaration (TermF syntax a) where
  customConnectDeclaration = connectDeclaration . termFOut

class IsDeclarationWithStrategy (strategy :: Strategy) syntax where
  connectDeclarationWithStrategy :: FreeVariables term => proxy strategy -> syntax term -> CallGraph -> CallGraph

instance IsDeclarationWithStrategy 'Default syntax where
  connectDeclarationWithStrategy _ _ = id

instance CustomIsDeclaration syntax => IsDeclarationWithStrategy 'Custom syntax where
  connectDeclarationWithStrategy _ = customConnectDeclaration

data Strategy = Default | Custom

type family IsDeclarationStrategy syntax where
  IsDeclarationStrategy Declaration.Function = 'Custom
  IsDeclarationStrategy Declaration.Method = 'Custom
  IsDeclarationStrategy (Union fs) = 'Custom
  IsDeclarationStrategy (TermF f a) = 'Custom
  IsDeclarationStrategy a = 'Default


instance Monoid CallGraph where
  mempty = empty
  mappend = overlay

instance Ord CallGraph where
  compare (CallGraph G.Empty)           (CallGraph G.Empty)           = EQ
  compare (CallGraph G.Empty)           _                             = LT
  compare _                             (CallGraph G.Empty)           = GT
  compare (CallGraph (G.Vertex a))      (CallGraph (G.Vertex b))      = compare a b
  compare (CallGraph (G.Vertex _))      _                             = LT
  compare _                             (CallGraph (G.Vertex _))      = GT
  compare (CallGraph (G.Overlay a1 a2)) (CallGraph (G.Overlay b1 b2)) = (compare `on` CallGraph) a1 b1 <> (compare `on` CallGraph) a2 b2
  compare (CallGraph (G.Overlay _  _))  _                             = LT
  compare _                             (CallGraph (G.Overlay _ _))   = GT
  compare (CallGraph (G.Connect a1 a2)) (CallGraph (G.Connect b1 b2)) = (compare `on` CallGraph) a1 b1 <> (compare `on` CallGraph) a2 b2
