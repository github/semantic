{-# LANGUAGE FlexibleContexts, OverloadedStrings, RecordWildCards #-}
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
import qualified Data.Core as Core
import           Data.File
import           Data.Foldable (fold)
import           Data.Function (fix)
import           Data.List.NonEmpty (nonEmpty)
import           Data.Loc
import qualified Data.Map as Map
import           Data.Name
import qualified Data.Set as Set
import           Data.Text (Text)
import           Prelude hiding (fail)

type ImportGraph = Map.Map Text (Set.Set Text)

data Value = Value
  { valueSemi  :: Semi
  , valueGraph :: ImportGraph
  }
  deriving (Eq, Ord, Show)

instance Semigroup Value where
  Value _ g1 <> Value _ g2 = Value Abstract (Map.unionWith (<>) g1 g2)

instance Monoid Value where
  mempty = Value Abstract mempty

data Semi
  = Closure Loc Name (Core.Core Name) Name
  -- FIXME: Bound String values.
  | String Text
  | Abstract
  deriving (Eq, Ord, Show)


importGraph :: [File (Core.Core Name)] -> (Heap Value, [File (Either (Loc, String) Value)])
importGraph
  = run
  . runFresh
  . runNaming (Root "import-graph")
  . runHeap
  . traverse runFile

runFile :: ( Carrier sig m
           , Effect sig
           , Member Fresh sig
           , Member Naming sig
           , Member (Reader FrameId) sig
           , Member (State (Heap Value)) sig
           )
        => File (Core.Core Name)
        -> m (File (Either (Loc, String) Value))
runFile file = traverse run file
  where run = runReader (fileLoc file)
            . runFailWithLoc
            . fmap fold
            . convergeTerm (fix (cacheTerm . eval importGraphAnalysis))

-- FIXME: decompose into a product domain and two atomic domains
importGraphAnalysis :: ( Alternative m
                       , Carrier sig m
                       , Member (Reader FrameId) sig
                       , Member (Reader Loc) sig
                       , Member (State (Heap Value)) sig
                       , MonadFail m
                       )
                    => Analysis Name Value m
importGraphAnalysis = Analysis{..}
  where alloc = pure
        bind _ _ = pure ()
        lookupEnv = pure . Just
        deref addr = gets (Map.lookup addr) >>= maybe (pure Nothing) (foldMapA (pure . Just)) . nonEmpty . maybe [] Set.toList
        assign addr ty = modify (Map.insertWith (<>) addr (Set.singleton ty))
        abstract _ name body = do
          loc <- ask
          FrameId parentAddr <- ask
          pure (Value (Closure loc name body parentAddr) mempty)
        apply eval (Value (Closure loc name body _) _) a = local (const loc) $ do
          addr <- alloc name
          assign addr a
          bind name addr
          eval body
        apply _ f _ = fail $ "Cannot coerce " <> show f <> " to function"
        unit = pure mempty
        bool _ = pure mempty
        asBool _ = pure True <|> pure False
        string s = pure (Value (String s) mempty)
        asString (Value (String s) _) = pure s
        asString _ = pure mempty
        frame = pure mempty
        edge Core.Import (Path to) = do
          Loc{locPath=from} <- ask
          () <$ pure (Value Abstract (Map.singleton from (Set.singleton to)))
        edge _ _ = pure ()
        _ ... m = m
