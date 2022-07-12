{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Analysis.Analysis.Exception
( Exception(..)
, ExcSet(..)
, exceptionTracing
, exceptionTracingIndependent
, fromExceptions
, var
, exc
, str
, subst
, nullExcSet
, freeVariablesForLine
, exceptionsForLine
, printExcSet
, refLines
  -- * Exception tracing analysis
, ExcC(..)
) where

import qualified Analysis.Carrier.Statement.State as A
import qualified Analysis.Carrier.Store.Monovariant as A
import           Analysis.Effect.Domain
import           Analysis.Effect.Env (Env)
import           Analysis.Effect.Store
import           Analysis.File
import           Analysis.FlowInsensitive (cacheTerm, convergeTerm)
import           Analysis.Module
import           Analysis.Name
import           Analysis.Reference
import           Control.Algebra
import           Control.Applicative (Alternative (..))
import           Control.Carrier.Reader
import           Control.Effect.Labelled
import           Control.Effect.State
import           Control.Monad (unless)
import           Data.Foldable (for_)
import qualified Data.Foldable as Foldable
import           Data.Function (fix)
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Source.Source as Source
import           Source.Span

-- | Names of exceptions thrown in the guest language and recorded by this analysis.
--
-- Not to be confused with exceptions thrown in Haskell itself.
data Exception = Exception { exceptionName :: Name, exceptionLines :: IntSet.IntSet }
  deriving (Eq, Ord, Show)

data FreeVariable = FreeVariable { freeVariableName :: Name, freeVariableLines :: IntSet.IntSet }
  deriving (Eq, Ord, Show)

-- | Sets whose elements are each a variable or an exception.
data ExcSet = ExcSet { freeVariables :: Set.Set FreeVariable, exceptions :: Set.Set Exception, strings :: Set.Set Text.Text }
  deriving (Eq, Ord, Show)

instance Semigroup ExcSet where
  ExcSet v1 e1 s1 <> ExcSet v2 e2 s2 = ExcSet (v1 <> v2) (e1 <> e2) (s1 <> s2)

instance Monoid ExcSet where
  mempty = ExcSet mempty mempty mempty

fromExceptions :: Foldable t => t Exception -> ExcSet
fromExceptions es = ExcSet mempty (Set.fromList (Foldable.toList es)) mempty

var :: FreeVariable -> ExcSet
var v = ExcSet (Set.singleton v) mempty mempty

exc :: Exception -> ExcSet
exc e = ExcSet mempty (Set.singleton e) mempty

str :: Text.Text -> ExcSet
str s = ExcSet mempty mempty (Set.singleton s)

subst  :: Name -> ExcSet -> ExcSet -> ExcSet
-- FIXME: this doesn't handle transitivity at all.
subst name (ExcSet _ es' _) (ExcSet fvs es ss) = ExcSet fvs'' (es <> es'') ss
  where
  (fvs'', es'') = foldMap combine fvs
  combine fv
    | freeVariableName fv == name = (mempty, Set.map (\ (Exception n _) -> Exception n (freeVariableLines fv)) es')
    | otherwise                   = (Set.singleton fv, mempty)


nullExcSet :: ExcSet -> Bool
nullExcSet e = null (freeVariables e) && null (exceptions e)

freeVariablesForLine :: Int -> ExcSet -> Set.Set FreeVariable
freeVariablesForLine l e = Set.filter (\ fv -> IntSet.member l (freeVariableLines fv)) (freeVariables e)

exceptionsForLine :: Int -> ExcSet -> Set.Set Exception
exceptionsForLine l e = Set.filter (\ ex -> IntSet.member l (exceptionLines ex)) (exceptions e)

printExcSet :: Source.Source -> ExcSet -> IO ()
printExcSet src e = for_ (zip [0..] (Source.lines src)) $ \ (i, line) -> do
  Text.putStr (keywords (Text.dropWhileEnd (== '\n') (Source.toText line)))
  let es  = exceptionsForLine    i e
      fvs = freeVariablesForLine i e
  unless (null es && null fvs) $ do
    Text.putStr " \ESC[30;1m# "
    Text.putStr ("{" <> union
      (  formatFreeVariables fvs
      <> formatExceptions    es ) <> "}" <> reset)
  Text.putStrLn mempty
  where
  keyword k s = Text.intercalate ("\ESC[34;1m" <> k <> reset) (Text.splitOn k s)
  keywords = keyword "raise" . keyword "import" . keyword "def" . keyword "pass"
  union = Text.intercalate ", "
  formatFreeVariables fvs  = map (\ fv -> "?" <> formatName (freeVariableName fv)) (Set.toList fvs)
  formatExceptions    excs = map (Text.pack . show . formatName . exceptionName) (Set.toList excs)
  reset = "\ESC[0m"

refLines :: Reference -> IntSet.IntSet
refLines (Reference _ (Span (Pos startLine _) (Pos endLine _))) = IntSet.fromAscList [startLine..endLine]

exceptionTracing
  :: Ord term
  => ( forall sig m
     .  (Has (Env A.MAddr) sig m, HasLabelled Store (Store A.MAddr ExcSet) sig m, Has (Dom ExcSet) sig m, Has (Reader Reference) sig m, Has A.Statement sig m)
     => (term -> m ExcSet)
     -> (term -> m ExcSet) )
  -> [File term]
  -> (A.MStore ExcSet, [File (Module ExcSet)])
exceptionTracing eval = run . A.runFiles (runFile eval)

exceptionTracingIndependent
  :: Ord term
  => ( forall sig m
     .  (Has (Env A.MAddr) sig m, HasLabelled Store (Store A.MAddr ExcSet) sig m, Has (Dom ExcSet) sig m, Has (Reader Reference) sig m, Has A.Statement sig m)
     => (term -> m ExcSet)
     -> (term -> m ExcSet) )
  -> File term
  -> (A.MStore ExcSet, File (Module ExcSet))
exceptionTracingIndependent eval = run . A.runStoreState . runFile eval

runFile
  :: ( Has (State (A.MStore ExcSet)) sig m
     , Ord term )
  => ( forall sig m
     .  (Has (Env A.MAddr) sig m, HasLabelled Store (Store A.MAddr ExcSet) sig m, Has (Dom ExcSet) sig m, Has (Reader Reference) sig m, Has A.Statement sig m)
     => (term -> m ExcSet)
     -> (term -> m ExcSet) )
  -> File term
  -> m (File (Module ExcSet))
runFile eval file = traverse run file where
  run
    = A.runStatement result
    . A.runEnv @ExcSet
    . runReader (fileRef file)
    . convergeTerm (A.runStore @ExcSet . runExcC . fix (cacheTerm . eval))
  result msgs sets = do
    exports <- gets @(A.MStore ExcSet) (fmap Foldable.fold . Map.mapKeys A.getMAddr . A.getMStore)
    let set = Foldable.fold sets
        imports = Set.fromList (map extractImport msgs)
    pure (Module (Foldable.foldl' (flip (uncurry subst)) set . Map.toList) imports exports (Set.map freeVariableName (freeVariables set)))
  extractImport (A.Import components) = name (Text.intercalate "." (Foldable.toList components))

newtype ExcC m a = ExcC { runExcC :: m a }
  deriving (Alternative, Applicative, Functor, Monad)

instance (Has (Reader Reference) sig m, Alternative m) => Algebra (Dom ExcSet :+: sig) (ExcC m) where
  alg hdl sig ctx = ExcC $ case sig of
    L dom   -> case dom of
      DVar n    -> do
        lines <- asks refLines
        pure $ var (FreeVariable n lines) <$ ctx
      DAbs _ b  -> runExcC (hdl (b mempty <$ ctx))
      DApp f a  -> pure $ f <> Foldable.fold a <$ ctx
      DInt _    -> pure nil
      DUnit     -> pure nil
      DBool _   -> pure nil
      DIf c t e -> fmap (mappend c) <$> runExcC (hdl (t <$ ctx) <|> hdl (e <$ ctx))
      DString s -> pure (str (Text.dropAround (== '"') s) <$ ctx)
      t :>>> u  -> pure (t <> u <$ ctx)
      DDie e    -> do
        lines <- asks refLines
        pure $ e{ strings = mempty } <> fromExceptions [Exception (name n) lines | n <- Set.toList (strings e)] <$ ctx
      where
      nil = (mempty :: ExcSet) <$ ctx

    R other -> alg (runExcC . hdl) other ctx
