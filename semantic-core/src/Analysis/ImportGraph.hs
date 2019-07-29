{-# LANGUAGE FlexibleContexts, RankNTypes, RecordWildCards, TypeApplications #-}
module Analysis.ImportGraph
( ImportGraph
, importGraph
, importGraphAnalysis
) where

import           Analysis.Eval
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative(..))
import           Control.Effect
import           Control.Effect.Fail
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad ((>=>))
import           Data.File
import           Data.Foldable (fold, for_)
import           Data.Function (fix)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name
import           Data.Proxy
import qualified Data.Set as Set
import           Data.Text (Text)
import           Prelude hiding (fail)

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
  = Closure Loc User term
  -- FIXME: Bound String values.
  | String Text
  | Abstract
  deriving (Eq, Ord, Show)


importGraph
  :: (Ord term, Show term)
  => (forall sig m
     .  (Carrier sig m, Member (Reader Loc) sig, MonadFail m)
     => Analysis term User (Value term) m
     -> (term -> m (Value term))
     -> (term -> m (Value term))
     )
  -> [File term]
  -> ( Heap User (Value term)
     , [File (Either (Loc, String) (Value term))]
     )
importGraph eval
  = run
  . runFresh
  . runHeap
  . traverse (runFile eval)

runFile
  :: ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap User (Value term))) sig
     , Ord  term
     , Show term
     )
  => (forall sig m
     .  (Carrier sig m, Member (Reader Loc) sig, MonadFail m)
     => Analysis term User (Value term) m
     -> (term -> m (Value term))
     -> (term -> m (Value term))
     )
  -> File term
  -> m (File (Either (Loc, String) (Value term)))
runFile eval file = traverse run file
  where run = runReader (fileLoc file)
            . runFailWithLoc
            . fmap fold
            . convergeTerm (Proxy @User) (fix (cacheTerm . eval importGraphAnalysis))

-- FIXME: decompose into a product domain and two atomic domains
importGraphAnalysis :: ( Alternative m
                       , Carrier sig m
                       , Member (Reader Loc) sig
                       , Member (State (Heap User (Value term))) sig
                       , MonadFail m
                       , Ord  term
                       , Show term
                       )
                    => Analysis term User (Value term) m
importGraphAnalysis = Analysis{..}
  where alloc = pure
        bind _ _ m = m
        lookupEnv = pure . Just
        deref addr = gets (Map.lookup addr >=> nonEmpty . Set.toList) >>= maybe (pure Nothing) (foldMapA (pure . Just))
        assign addr v = modify (Map.insertWith (<>) addr (Set.singleton v))
        abstract _ name body = do
          loc <- ask
          pure (Value (Closure loc name body) mempty)
        apply eval (Value (Closure loc name body) _) a = local (const loc) $ do
          addr <- alloc name
          assign addr a
          bind name addr (eval body)
        apply _ f _ = fail $ "Cannot coerce " <> show f <> " to function"
        unit = pure mempty
        bool _ = pure mempty
        asBool _ = pure True <|> pure False
        string s = pure (Value (String s) mempty)
        asString (Value (String s) _) = pure s
        asString _ = pure mempty
        record fields = do
          for_ fields $ \ (k, v) -> do
            addr <- alloc k
            assign addr v
          pure (Value Abstract (foldMap (valueGraph . snd) fields))
        _ ... m = pure (Just m)
