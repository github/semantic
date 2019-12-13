{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, ScopedTypeVariables, TypeApplications #-}
module Analysis.ImportGraph
( ImportGraph
, importGraph
, importGraphAnalysis
) where

import           Analysis.Analysis
import           Analysis.Carrier.Env.Monovariant
import qualified Analysis.Carrier.Heap.Monovariant as A
import           Analysis.File
import           Analysis.FlowInsensitive
import           Analysis.Name
import           Control.Applicative (Alternative(..))
import           Control.Algebra
import           Control.Carrier.Fail.WithLoc
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Data.Foldable (fold, for_)
import           Data.Function (fix)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import           Prelude hiding (fail)
import           Source.Span
import qualified System.Path as Path

type ImportGraph = Map.Map Text (Set.Set Text)

data Value term = Value
  { valueSemi  :: Semi term
  , valueGraph :: ImportGraph
  }
  deriving (Eq, Ord, Show)

instance Semigroup (Value term) where
  Value _ g1 <> Value _ g2 = Value Abstract (Map.unionWith (<>) g1 g2)

instance Monoid (Value term) where
  mempty = Value Abstract mempty

data Semi term
  = Closure Path.AbsRelFile Span Name term
  -- FIXME: Bound String values.
  | String Text
  | Abstract
  deriving (Eq, Ord, Show)


importGraph
  :: (Ord (term Name), Show (term Name))
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => Analysis term Name (Value (term Name)) m
     -> (term Name -> m (Value (term Name)))
     -> (term Name -> m (Value (term Name)))
     )
  -> [File (term Name)]
  -> ( Heap (Value (term Name))
     , [File (Either (Path.AbsRelFile, Span, String) (Value (term Name)))]
     )
importGraph eval
  = run
  . evalFresh 0
  . runHeap
  . traverse (runFile eval)

runFile
  :: forall term m sig
  .  ( Effect sig
     , Has Fresh sig m
     , Has (State (Heap (Value (term Name)))) sig m
     , Ord  (term Name)
     , Show (term Name)
     )
  => (forall sig m
     .  (Has (Reader Path.AbsRelFile) sig m, Has (Reader Span) sig m, MonadFail m)
     => Analysis term Name (Value (term Name)) m
     -> (term Name -> m (Value (term Name)))
     -> (term Name -> m (Value (term Name)))
     )
  -> File (term Name)
  -> m (File (Either (Path.AbsRelFile, Span, String) (Value (term Name))))
runFile eval file = traverse run file
  where run = runReader (filePath file)
            . runReader (fileSpan file)
            . runEnv
            . runFail
            . fmap fold
            . convergeTerm 0 (A.runHeap @Name @(Value (term Name)) . fix (cacheTerm . eval importGraphAnalysis))

-- FIXME: decompose into a product domain and two atomic domains
importGraphAnalysis
  :: ( Alternative m
     , Has (Env Name) sig m
     , Has (A.Heap Name (Value (term Name))) sig m
     , Has (Reader Path.AbsRelFile) sig m
     , Has (Reader Span) sig m
     , MonadFail m
     , Show (term Name)
     )
  => Analysis term Name (Value (term Name)) m
importGraphAnalysis = Analysis{..}
  where abstract _ name body = do
          path <- ask
          span <- ask
          pure (Value (Closure path span name body) mempty)
        apply eval (Value (Closure path span name body) _) a = local (const path) . local (const span) $ do
          addr <- alloc @Name name
          A.assign addr a
          bind name addr (eval body)
        apply _ f _ = fail $ "Cannot coerce " <> show f <> " to function"
        string s = pure (Value (String s) mempty)
        asString (Value (String s) _) = pure s
        asString _ = pure mempty
        record fields = do
          for_ fields $ \ (k, v) -> do
            addr <- alloc @Name k
            A.assign addr v
          pure (Value Abstract (foldMap (valueGraph . snd) fields))
        _ ... m = pure (Just m)
