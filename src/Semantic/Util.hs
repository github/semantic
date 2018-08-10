{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util where

import Prelude hiding (id, (.), readFile)

import Control.Category
import           Analysis.Abstract.Caching
import           Analysis.Abstract.Collecting
import           Control.Abstract.Matching
import           Control.Abstract
import           Control.Abstract.Matching
import           Control.Exception (displayException)
import           Control.Monad.Effect.Trace (runPrintingTrace)
import           Control.Arrow
import           Control.Rule
import           Control.Rule.Engine.Builtin
import           Data.Abstract.Address
import           Data.Abstract.Evaluatable
import           Data.Abstract.Module
import qualified Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Package
import           Data.Abstract.Value.Concrete as Concrete
import           Data.Abstract.Value.Type as Type
import           Data.Blob
import           Data.Coerce
import           Data.Functor.Foldable
import           Data.Graph (topologicalSort)
import           Data.History
import           Data.Machine
import qualified Data.Language as Language
import           Data.List (uncons)
import           Data.Project hiding (readFile)
import           Data.Record
import           Data.Sum (weaken)
import qualified Data.Sum as Sum
import qualified Data.Syntax.Literal as Literal
import           Data.Term
import           Language.Haskell.HsColour
import           Language.Haskell.HsColour.Colourise
import           Parsing.Parser
import           Prologue hiding (weaken)
import           Refactoring.Core
import           Reprinting.Tokenize
import           Reprinting.Translate
import           Reprinting.Typeset
import           Semantic.Config
import           Semantic.Graph
import           Semantic.IO as IO
import qualified Data.Sum as Sum
import           Semantic.Task
import           Semantic.Telemetry (LogQueue, StatQueue)
import           System.Exit (die)
import           System.FilePath.Posix (takeDirectory)
import           Text.Show (showListWith)
import           Text.Show.Pretty (ppShow)


import qualified Debug.Trace as Debug

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

newtype UtilEff address a = UtilEff
  { runUtilEff :: Eff '[ Function address (Value address (UtilEff address))
                       , Exc (LoopControl address)
                       , Exc (Return address)
                       , Env address
                       , Deref address (Value address (UtilEff address))
                       , Allocator address (Value address (UtilEff address))
                       , Reader ModuleInfo
                       , Modules address
                       , Reader (ModuleTable (NonEmpty (Module (ModuleResult address))))
                       , Reader Span
                       , Reader PackageInfo
                       , Resumable (ValueError address (UtilEff address))
                       , Resumable (AddressError address (Value address (UtilEff address)))
                       , Resumable ResolutionError
                       , Resumable EvalError
                       , Resumable (EnvironmentError address)
                       , Resumable (Unspecialized (Value address (UtilEff address)))
                       , Resumable (LoadError address)
                       , Trace
                       , Fresh
                       , State (Heap address Latest (Value address (UtilEff address)))
                       , Lift IO
                       ] a
  }

checking
  = runM @_ @IO
  . runState (lowerBound @(Heap Monovariant All Type))
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

evalGoProject         = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Go)         goParser         Language.Go
evalRubyProject       = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Ruby)       rubyParser       Language.Ruby
evalPHPProject        = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.PHP)        phpParser        Language.PHP
evalPythonProject     = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.Python)     pythonParser     Language.Python
evalJavaScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.JavaScript) typescriptParser Language.JavaScript
evalTypeScriptProject = justEvaluating <=< evaluateProject (Proxy :: Proxy 'Language.TypeScript) typescriptParser Language.TypeScript

typecheckGoFile = checking <=< evaluateProjectWithCaching (Proxy :: Proxy 'Language.Go) goParser Language.Go

callGraphProject parser proxy lang opts paths = runTaskWithOptions opts $ do
  blobs <- catMaybes <$> traverse readFile (flip File lang <$> paths)
  package <- parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs lang [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  x <- runCallGraph proxy False modules package
  pure (x, (() <$) <$> modules)

callGraphRubyProject = callGraphProject rubyParser (Proxy @'Language.Ruby) Language.Ruby debugOptions


renameKey :: (Literal.TextElement :< fs, Literal.KeyValue :< fs, Apply Functor fs) => Term (Sum fs) (Record (History ': fields)) -> Term (Sum fs) (Record (History ': fields))
renameKey p = case Sum.project (termOut p) of
  Just (Literal.KeyValue k v)
    | Just (Literal.TextElement x) <- Sum.project (termOut k)
    , x == "\"foo\""
    -> let newKey = termIn (termAnnotation k) (inject (Literal.TextElement "\"fooA\""))
       in remark Refactored (termIn (termAnnotation p) (inject (Literal.KeyValue newKey v)))
  _ -> Term (fmap renameKey (unTerm p))

arrayMatcher :: forall fs ann term . (Literal.Array :< fs, term ~ Term (Sum fs) ann)
            => Matcher term (Literal.Array term)
arrayMatcher = matchM hash target
  where hash :: term -> Maybe (Literal.Array term)
        hash = projectTerm

increaseNumbers :: (Literal.Float :< fs, Apply Functor fs) => Term (Sum fs) (Record (History ': fields)) -> Term (Sum fs) (Record (History ': fields))
increaseNumbers p = case Sum.project (termOut p) of
  Just (Literal.Float t) -> remark Refactored (termIn (termAnnotation p) (inject (Literal.Float (t <> "0"))))
  Nothing                -> Term (fmap increaseNumbers (unTerm p))

hashMatcher :: forall fs ann term . (Literal.Hash :< fs, term ~ Term (Sum fs) ann)
            => Matcher term (Literal.Hash term)
hashMatcher = matchM hash target
  where hash :: term -> Maybe (Literal.Hash term)
        hash = projectTerm

testHashMatcher = do
  (src, tree) <- testJSONFile
  runMatcher hashMatcher tree

findHashes :: ( Apply Functor syntax
              , Apply Foldable syntax
              , Literal.Hash :< syntax
              , term ~ Term (Sum syntax) ann
              )
           => Rule eff term (Either term (term, Literal.Hash term))
findHashes = fromMatcher "findHashes" hashMatcher

addKVPair :: forall effs syntax ann fields term
             . ( Apply Functor syntax
               , Apply Foldable syntax
               , Literal.Float :< syntax
               , Literal.Hash :< syntax
               , Literal.Array :< syntax
               , Literal.TextElement :< syntax
               , Literal.KeyValue :< syntax
               , ann ~ Record (History ': fields)
               , term ~ Term (Sum syntax) ann
               )
           => Rule effs (Either term (term, Literal.Hash term)) term
addKVPair = fromPlan "addKVPair" $ do
  t <- await
  Data.Machine.yield (either id injKVPair t)
  where
    injKVPair :: (term, Literal.Hash term) -> term
    injKVPair (origTerm, Literal.Hash xs) =
      -- remark Refactored (termIn (termAnnotation origTerm) (inject (Literal.Hash (xs <> [newItem]))))
      remark Refactored (injectTerm ann (Literal.Hash [newItem]))
      where
        newItem = termIn gen (inject (Literal.KeyValue k v))
        k = termIn gen (inject (Literal.TextElement "added"))
        v = termIn gen (inject (Literal.Array []))
        gen = Generated :. rtail ann
        ann = termAnnotation origTerm

testAddKVPair = do
  (src, tree) <- testJSONFile
  tagged <- runM $ ensureAccurateHistory <$> cata (toAlgebra (addKVPair . findHashes)) (mark Pristine tree)
  let toks = tokenizing src tagged
  pure (toks, tagged)

testAddKVPair' = do
  res <- translating (Proxy @'Language.JSON) . fst <$> testAddKVPair
  putStrLn (either show (show . typeset) res)

floatMatcher :: forall fs ann term . (Literal.Float :< fs, term ~ Term (Sum fs) ann)
             => Matcher term (Literal.Float term)
floatMatcher = matchM float target
  where float :: term -> Maybe (Literal.Float term)
        float = projectTerm

testFloatMatcher = do
  (src, tree) <- testJSONFile
  runMatcher floatMatcher tree

findFloats :: ( Apply Functor syntax
              , Apply Foldable syntax
              , Literal.Float :< syntax
              , term ~ Term (Sum syntax) ann
              )
           => Rule effs term (Either term (term, Literal.Float term))
findFloats = fromMatcher "test" floatMatcher

overwriteFloats :: forall effs syntax ann fields term . ( Apply Functor syntax
               , Apply Foldable syntax
               , Literal.Float :< syntax
               , ann ~ Record (History ': fields)
               , term ~ Term (Sum syntax) ann
               )
           => Rule effs (Either term (term, Literal.Float term)) term
overwriteFloats = fromPlan "overwritingFloats" $ do
  t <- await
  Data.Machine.yield (either id injFloat t)
  where injFloat :: (term, Literal.Float term) -> term
        injFloat (term, _) = remark Refactored (termIn (termAnnotation term) (inject (Literal.Float "0")))

testOverwriteFloats = do
  (src, tree) <- testJSONFile
  tagged <- runM $ ensureAccurateHistory <$> cata (toAlgebra (overwriteFloats . findFloats)) (mark Pristine tree)
  let toks = tokenizing src tagged
  pure (toks, tagged)

testOverwriteFloats' = do
  res <- translating (Proxy @'Language.JSON) . fst <$> testOverwriteFloats
  putStrLn (either show (show . typeset) res)

{-

             Hash
  term     /-------> refactor ------->\   term
--------->/                           |---------->
          \                          /
           \------> do nothing ----->/
             non-Hashes

-}


-- addKVPair :: forall fs fields .
--              (Apply Functor fs, Literal.Array :< fs, Literal.Hash :< fs, Literal.TextElement :< fs, Literal.KeyValue :< fs, Literal.Float :< fs)
--           => Term (Sum fs) (Record (History ': fields))
--           -> Term (Sum fs) (Record (History ': fields))
-- addKVPair p = case Sum.project (termOut p) of
--   Just (Literal.Hash h) -> termIn (Data.History.overwrite Modified (rhead (termAnnotation p)) :. rtail (annotation p)) (addToHash h)
--   Nothing -> Term (fmap addKVPair (unTerm p))
--   where
--     addToHash :: [Term (Sum fs) (Record (History : fields))] -> Sum fs (Term (Sum fs) (Record (History : fields)))
--     addToHash pairs = inject . Literal.Hash $ (pairs ++ [newItem])
--     newItem :: Term (Sum fs) (Record (History : fields))
--     newItem = termIn gen (inject (Literal.KeyValue fore aft))
--     fore = termIn gen (inject (Literal.TextElement "fore"))
--     aft = termIn gen (inject (Literal.TextElement "aft"))
--     gen = Generated :. rtail (annotation p)
--     item = inject (Literal.KeyValue (inject (Literal.TextElement "added")) (inject (Literal.Array [])))

testJSONFile = do
  let path = "test/fixtures/javascript/reprinting/map.json"
  src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
  tree <- parseFile jsonParser path
  pure (src, tree)

testTokenizer = do
  (src, tree) <- testJSONFile

  let tagged = ensureAccurateHistory $ renameKey (mark Pristine tree)
  let toks = tokenizing src tagged
  pure (toks, tagged)

testTranslator = translating (Proxy @'Language.JSON) . fst <$> testTokenizer

testTypeSet = do
  res <- testTranslator
  putStrLn (either show (show . typeset) res)


-- Evaluate a project consisting of the listed paths.
evaluateProject proxy parser lang paths = withOptions debugOptions $ \ config logger statter ->
  evaluateProject' (TaskConfig config logger statter) proxy parser lang paths

data TaskConfig = TaskConfig Config LogQueue StatQueue

evaluateProject' (TaskConfig config logger statter) proxy parser lang paths = either (die . displayException) pure <=< runTaskWithConfig config logger statter $ do
  blobs <- catMaybes <$> traverse readFile (flip File lang <$> paths)
  package <- fmap quieterm <$> parsePackage parser (Project (takeDirectory (maybe "/" fst (uncons paths))) blobs lang [])
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  trace $ "evaluating with load order: " <> show (map (modulePath . moduleInfo) modules)
  pure (runTermEvaluator @_ @_ @(Value Precise (UtilEff Precise))
       (runReader (packageInfo package)
       (runReader (lowerBound @Span)
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Precise)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (evaluate proxy id withTermSpans (Concrete.runFunction coerce coerce) modules))))))


evaluateProjectWithCaching proxy parser lang path = runTaskWithOptions debugOptions $ do
  project <- readProject Nothing path lang []
  package <- fmap quieterm <$> parsePackage parser project
  modules <- topologicalSort <$> runImportGraphToModules proxy package
  pure (runReader (packageInfo package)
       (runReader (lowerBound @Span)
       (runReader (lowerBound @(ModuleTable (NonEmpty (Module (ModuleResult Monovariant)))))
       (raiseHandler (runModules (ModuleTable.modulePaths (packageModules package)))
       (evaluate proxy id withTermSpans Type.runFunction modules)))))


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
