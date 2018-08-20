-- {-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util where

import Prelude hiding (id, (.), readFile)

import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Control.Abstract
import           Control.Abstract.Matching
import           Control.Category
import           Control.Exception (displayException)
import           Control.Monad.Effect.Trace (runPrintingTrace)
import           Control.Rule
import           Data.Abstract.Address.Monovariant as Monovariant
import           Data.Abstract.Address.Precise as Precise
import           Data.Abstract.BaseError (BaseError (..))
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import qualified Data.ByteString.Char8 as BC
import           Data.Coerce
import           Data.Graph (topologicalSort)
import           Data.History
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Machine
import           Data.Project hiding (readFile)
import           Data.Quieterm (quieterm)
import           Data.Record
import qualified Data.Source as Source
import           Data.Sum (weaken)
import qualified Data.Sum as Sum
import qualified Data.Syntax.Literal as Literal
import           Data.Term
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Language.JSON.Translate
import           Matching.Core
import           Parsing.Parser
import           Prologue hiding (weaken)
import           Reprinting.Pipeline
import           Semantic.Config
import           Semantic.Graph
import           Semantic.IO as IO
import           Semantic.Task
import           Semantic.Telemetry (LogQueue, StatQueue)
import           System.Exit (die)
import           System.FilePath.Posix (takeDirectory)
import           Text.Show.Pretty (ppShow)

justEvaluating
  = runM
  . runState lowerBound
  . runFresh 0
  . runPrintingTrace
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runEnvironmentError
  . runEvalError
  . runResolutionError
  . runAddressError
  . runValueError

newtype UtilEff a = UtilEff
  { runUtilEff :: Eff '[ Function Precise (Value Precise UtilEff)
                       , Exc (LoopControl Precise)
                       , Exc (Return Precise)
                       , Env Precise
                       , Deref (Value Precise UtilEff)
                       , Allocator Precise
                       , Reader ModuleInfo
                       , Modules Precise
                       , Reader (ModuleTable (NonEmpty (Module (ModuleResult Precise))))
                       , Reader Span
                       , Reader PackageInfo
                       , Resumable (BaseError (ValueError Precise UtilEff))
                       , Resumable (BaseError (AddressError Precise (Value Precise UtilEff)))
                       , Resumable (BaseError ResolutionError)
                       , Resumable (BaseError EvalError)
                       , Resumable (BaseError (EnvironmentError Precise))
                       , Resumable (BaseError (UnspecializedError (Value Precise UtilEff)))
                       , Resumable (BaseError (LoadError Precise))
                       , Trace
                       , Fresh
                       , State (Heap Precise (Value Precise UtilEff))
                       , Lift IO
                       ] a
  }

checking
  = runM @_ @IO
  . runState (lowerBound @(Heap Monovariant Type))
  . runFresh 0
  . runPrintingTrace
  . runTermEvaluator @_ @Monovariant @Type
  . caching
  . providingLiveSet
  . fmap reassociate
  . runLoadError
  . runUnspecialized
  . runResolutionError
  . runEnvironmentError
  . runEvalError
  . runAddressError
  . runTypes

evalGoProject         = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Go)         goParser
evalRubyProject       = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Ruby)       rubyParser
evalPHPProject        = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.PHP)        phpParser
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser
evalJavaScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.JavaScript) typescriptParser
evalTypeScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser

typecheckGoFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Go) goParser
typecheckRubyFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Ruby) rubyParser

callGraphProject parser proxy opts paths = runTaskWithOptions opts $ do
  blobs <- catMaybes <$> traverse readFile (flip File (Language.reflect proxy) <$> paths)
  package <- parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  x <- runCallGraph proxy False modules package
  pure (x, (() <$) <$> modules)

callGraphRubyProject = callGraphProject rubyParser (Proxy @'Language.Ruby) debugOptions


-- Evaluate a project consisting of the listed paths.
evaluateProject proxy parser paths = withOptions debugOptions $ \ config logger statter ->
  evaluateProject' (TaskConfig config logger statter) proxy parser paths

data TaskConfig = TaskConfig Config LogQueue StatQueue

evaluateProject' (TaskConfig config logger statter) proxy parser paths = either (die . displayException) pure <=< runTaskWithConfig config logger statter $ do
  blobs <- catMaybes <$> traverse readFile (flip File (Language.reflect proxy) <$> paths)
  package <- fmap quieterm <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs (Language.reflect proxy) [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (runTermEvaluator @_ @_ @(Value Precise UtilEff)
       (runReader (packageInfo package)
       (runReader (lowerBound @Span)
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (evaluate proxy id withTermSpans (Precise.runAllocator . Precise.runDeref) (Concrete.runFunction coerce coerce) modules))))))


evaluateProjectWithCaching proxy parser path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path (Language.reflect proxy) []
  package <- fmap quieterm <$> parsePackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  pure (runReader (packageInfo package)
       (runReader (lowerBound @Span)
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Monovariant)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (evaluate proxy id withTermSpans (Monovariant.runAllocator . Monovariant.runDeref) Type.runFunction modules)))))


parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)

blob :: FilePath -> IO Blob
blob = runTask . readBlob . file


mergeExcs :: Either (SomeExc (Sum excs)) (Either (SomeExc exc) result) -> Either (SomeExc (Sum (exc ': excs))) result
mergeExcs = either (\ (SomeExc sum) -> Left (SomeExc (weaken sum))) (either (\ (SomeExc exc) -> Left (SomeExc (inject exc))) Right)

reassociate :: Either (SomeExc exc1) (Either (SomeExc exc2) (Either (SomeExc exc3) (Either (SomeExc exc4) (Either (SomeExc exc5) (Either (SomeExc exc6) (Either (SomeExc exc7) result)))))) -> Either (SomeExc (Sum '[exc7, exc6, exc5, exc4, exc3, exc2, exc1])) result
reassociate = mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . mergeExcs . Right


prettyShow :: Show a => a -> IO ()
prettyShow = putStrLn . hscolour TTY defaultColourPrefs False False "" False . ppShow


--
-- Code rewriting experiments
--

testJSONFile = do
  let path = "test/fixtures/javascript/reprinting/map.json"
  src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
  tree <- parseFile jsonParser path
  pure (src, tree)

renameKey :: (Literal.TextElement :< fs, Literal.KeyValue :< fs, Apply Functor fs) => Term (Sum fs) (Record (History ': fields)) -> Term (Sum fs) (Record (History ': fields))
renameKey p = case projectTerm p of
  Just (Literal.KeyValue k v)
    | Just (Literal.TextElement x) <- Sum.project (termOut k)
    , x == "\"foo\""
    -> let newKey = termIn (termAnnotation k) (inject (Literal.TextElement "\"fooA\""))
       in remark Refactored (termIn (termAnnotation p) (inject (Literal.KeyValue newKey v)))
  _ -> Term (fmap renameKey (unTerm p))

testRenameKey = do
  (src, tree) <- testJSONFile
  let tagged = renameKey (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

increaseNumbers :: (Literal.Float :< fs, Apply Functor fs) => Term (Sum fs) (Record (History ': fields)) -> Term (Sum fs) (Record (History ': fields))
increaseNumbers p = case Sum.project (termOut p) of
  Just (Literal.Float t) -> remark Refactored (termIn (termAnnotation p) (inject (Literal.Float (t <> "0"))))
  Nothing                -> Term (fmap increaseNumbers (unTerm p))

findHashes :: (Literal.Hash :< syntax , term ~ Term (Sum syntax) ann) => Rule eff term (Either term (term, Literal.Hash term))
findHashes = fromMatcher "findHashes" matchHash

addKVPair :: forall effs syntax ann fields term .
  ( Apply Functor syntax
  , Literal.Hash :< syntax
  , Literal.Array :< syntax
  , Literal.TextElement :< syntax
  , Literal.KeyValue :< syntax
  , ann ~ Record (History ': fields)
  , term ~ Term (Sum syntax) ann
  ) =>
  Rule effs (Either term (term, Literal.Hash term)) term
addKVPair = fromPlan "addKVPair" $ do
  t <- await
  Data.Machine.yield (either id injKVPair t)
  where
    injKVPair :: (term, Literal.Hash term) -> term
    injKVPair (origTerm, Literal.Hash xs) =
      remark Refactored (injectTerm ann (Literal.Hash (xs <> [newItem])))
      where
        newItem = termIn ann (inject (Literal.KeyValue k v))
        k = termIn ann (inject (Literal.TextElement "\"added\""))
        v = termIn ann (inject (Literal.Array []))
        ann = termAnnotation origTerm

testAddKVPair = do
  (src, tree) <- testJSONFile
  tagged <- runM $ cata (toAlgebra (addKVPair . findHashes)) (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

findFloats :: ( Literal.Float :< syntax , term ~ Term (Sum syntax) ann ) => Rule effs term (Either term (term, Literal.Float term))
findFloats = fromMatcher "test" matchFloat

overwriteFloats :: forall effs syntax ann fields term .
  ( Apply Functor syntax
  , Literal.Float :< syntax
  , ann ~ Record (History ': fields)
  , term ~ Term (Sum syntax) ann
  ) =>
  Rule effs (Either term (term, Literal.Float term)) term
overwriteFloats = fromPlan "overwritingFloats" $ do
  t <- await
  Data.Machine.yield (either id injFloat t)
  where injFloat :: (term, Literal.Float term) -> term
        injFloat (term, _) = remark Refactored (termIn (termAnnotation term) (inject (Literal.Float "0")))

testOverwriteFloats = do
  (src, tree) <- testJSONFile
  tagged <- runM $ cata (toAlgebra (overwriteFloats . findFloats)) (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

findKV ::
  ( Literal.KeyValue :< syntax
  , Literal.TextElement :< syntax
  , term ~ Term (Sum syntax) ann
  ) =>
  Text -> Rule effs term (Either term (term, Literal.KeyValue term))
findKV name = fromMatcher "findKV" (kvMatcher name)

kvMatcher :: forall fs ann term .
  ( Literal.KeyValue :< fs
  , Literal.TextElement :< fs
  , term ~ Term (Sum fs) ann
  ) =>
  Text -> Matcher term (Literal.KeyValue term)
kvMatcher name = matchM projectTerm target <* matchKey where
  matchKey
    = match Literal.key $
        match Literal.textElementContent $
          ensure (== name)

changeKV :: forall effs syntax ann fields term .
  ( Apply Functor syntax
  , Literal.KeyValue :< syntax
  , Literal.Array :< syntax
  , Literal.Float :< syntax
  , ann ~ Record (History ': fields)
  , term ~ Term (Sum syntax) ann
  ) =>
  Rule effs (Either term (term, Literal.KeyValue term)) term
changeKV = fromFunction "changeKV" $ either id injKV
  where
    injKV :: (term, Literal.KeyValue term) -> term
    injKV (term, Literal.KeyValue k v) = case projectTerm v of
      Just (Literal.Array elems) -> remark Refactored (termIn ann (inject (Literal.KeyValue k (newArray elems))))
      _ -> term
      where newArray xs = termIn ann (inject (Literal.Array (xs <> [float])))
            float = termIn ann (inject (Literal.Float "4"))
            ann = termAnnotation term

testChangeKV = do
  (src, tree) <- testJSONFile
  tagged <- runM $ cata (toAlgebra (changeKV . findKV "\"bar\"")) (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

testPipeline = do
  (src, tree) <- testJSONFile
  printToTerm $ runReprinter src defaultJSONPipeline (mark Refactored tree)

printToTerm res = either (putStrLn . show) (BC.putStr . Source.sourceBytes) res
