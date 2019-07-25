{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards, TypeApplications #-}
module Analysis.ScopeGraph
( ScopeGraph
, Entry(..)
, scopeGraph
, scopeGraphAnalysis
) where

import           Analysis.Eval
import           Analysis.FlowInsensitive
import           Control.Applicative (Alternative (..))
import           Control.Effect.Carrier
import           Control.Effect.Fail
import           Control.Effect.Fresh
import           Control.Effect.Reader
import           Control.Effect.State
import qualified Data.Core as Core
import           Data.File
import           Data.Foldable (fold)
import           Data.Function (fix)
import           Data.List.NonEmpty
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Term
import           Prelude hiding (fail)

data Entry = Entry
  { entrySymbol :: Text
  , entryLoc    :: Loc
  }
  deriving (Eq, Ord, Show)

type ScopeGraph = Map.Map Entry (Set.Set Entry)


data Value = Value
  { valueSemi  :: Semi
  , valueGraph :: ScopeGraph
  }
  deriving (Eq, Ord, Show)

instance Semigroup Value where
  Value _ g1 <> Value _ g2 = Value Abstract (Map.unionWith (<>) g1 g2)

instance Monoid Value where
  mempty = Value Abstract mempty

data Semi
  = Closure Loc User (Term Core.Core User)
  | Abstract
  deriving (Eq, Ord, Show)


scopeGraph :: [File (Term Core.Core User)] -> (Heap User Value, [File (Either (Loc, String) Value)])
scopeGraph
  = run
  . runFresh
  . runHeap "__semantic_root"
  . traverse runFile

runFile
  :: ( Carrier sig m
     , Effect sig
     , Member Fresh sig
     , Member (State (Heap User Value)) sig
     )
  => File (Term Core.Core User)
  -> m (File (Either (Loc, String) Value))
runFile file = traverse run file
  where run = runReader (fileLoc file)
            . runFailWithLoc
            . fmap fold
            . convergeTerm (fix (cacheTerm . eval scopeGraphAnalysis))

-- FIXME: decompose into a product domain and two atomic domains
scopeGraphAnalysis
  :: ( Alternative m
     , Carrier sig m
     , Member (Reader Loc) sig
     , Member (State (Heap User Value)) sig
     , MonadFail m
     )
  => Analysis User Value m
scopeGraphAnalysis = Analysis{..}
  where alloc = pure
        bind _ _ m = m
        lookupEnv = pure . Just
        deref addr = gets (Map.lookup addr) >>= maybe (pure Nothing) (foldMapA (pure . Just)) . nonEmpty . maybe [] (Set.toList @Value)
        assign addr ty = modify (Map.insertWith (<>) addr (Set.singleton ty))
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
        string _ = pure mempty
        asString _ = pure mempty
        record fields = pure (Value Abstract (foldMap (valueGraph . snd) fields))
        _ ... m = pure (Just m)
