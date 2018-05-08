{-# LANGUAGE DeriveAnyClass #-}
module Language.Ruby.Syntax where

import           Control.Monad (unless)
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.ModuleTable as ModuleTable
import           Data.Abstract.Path
import qualified Data.ByteString.Char8 as BC
import qualified Data.Language as Language
import           Diffing.Algorithm
import           Prelude hiding (fail)
import           Prologue
import           System.FilePath.Posix


-- TODO: Fully sort out ruby require/load mechanics
--
-- require "json"
resolveRubyName :: Members '[ Reader (ModuleTable [M.Module term])
                            , Resumable ResolutionError
                            ] effects
                => ByteString
                -> Evaluator location term value effects M.ModulePath
resolveRubyName name = do
  let name' = cleanNameOrPath name
  let paths = [name' <.> "rb"]
  modulePath <- resolve paths
  maybe (throwResumable $ NotFoundError name' paths Language.Ruby) pure modulePath

-- load "/root/src/file.rb"
resolveRubyPath :: Members '[ Reader (ModuleTable [M.Module term])
                            , Resumable ResolutionError
                            ] effects
                => ByteString
                -> Evaluator location term value effects M.ModulePath
resolveRubyPath path = do
  let name' = cleanNameOrPath path
  modulePath <- resolve [name']
  maybe (throwResumable $ NotFoundError name' [name'] Language.Ruby) pure modulePath

cleanNameOrPath :: ByteString -> String
cleanNameOrPath = BC.unpack . dropRelativePrefix . stripQuotes

data Send a = Send { sendReceiver :: Maybe a, sendSelector :: Maybe a, sendArgs :: [a], sendBlock :: Maybe a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Send where liftEq = genericLiftEq
instance Ord1 Send where liftCompare = genericLiftCompare
instance Show1 Send where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Send where
  eval Send{..} = do
    let sel = case sendSelector of
          Just sel -> subtermValue sel
          Nothing -> variable (name "call")
    func <- maybe sel (flip evaluateInScopedEnv sel . subtermValue) sendReceiver
    call func (map subtermValue sendArgs) -- TODO pass through sendBlock

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval (Require _ x) = do
    name <- subtermValue x >>= asString
    path <- resolveRubyName name
    traceResolve name path
    (importedEnv, v) <- isolate (doRequire path)
    modifyEnv (`mergeNewer` importedEnv)
    pure v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

doRequire :: ( AbstractValue location value effects
             , Members '[ EvalModule term value
                        , Loaded location value
                        , Reader (UnevaluatedModules term)
                        , Resumable (LoadError term)
                        , Resumable ResolutionError
                        , State (Environment location value)
                        , State (Exports location value)
                        , Trace
                        ] effects
             )
          => M.ModulePath
          -> Evaluator location term value effects (Environment location value, value)
doRequire path = do
  result <- join <$> lookupModule path
  case result of
    Nothing       -> (,) . maybe emptyEnv fst <$> load path <*> boolean True
    Just (env, _) -> (,) env                  <$>               boolean False


newtype Load a = Load { loadArgs :: [a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Load where liftEq = genericLiftEq
instance Ord1 Load where liftCompare = genericLiftCompare
instance Show1 Load where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Load where
  eval (Load [x]) = do
    path <- subtermValue x >>= asString
    doLoad path False
  eval (Load [x, wrap]) = do
    path <- subtermValue x >>= asString
    shouldWrap <- subtermValue wrap >>= asBool
    doLoad path shouldWrap
  eval (Load _) = raise (fail "invalid argument supplied to load, path is required")

doLoad :: ( AbstractValue location value effects
          , Members '[ EvalModule term value
                     , Loaded location value
                     , Reader (UnevaluatedModules term)
                     , Resumable (LoadError term)
                     , Resumable ResolutionError
                     , State (Environment location value)
                     , State (Exports location value)
                     , Trace
                     ] effects
          )
       => ByteString
       -> Bool
       -> Evaluator location term value effects value
doLoad path shouldWrap = do
  path' <- resolveRubyPath path
  traceResolve path path'
  importedEnv <- maybe emptyEnv fst <$> isolate (load path')
  unless shouldWrap $ modifyEnv (mergeEnvs importedEnv)
  boolean Prelude.True -- load always returns true. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-load

-- TODO: autoload

data Class a = Class { classIdentifier :: !a, classSuperClass :: !(Maybe a), classBody :: !a }
  deriving (Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class where
  eval Class{..} = do
    super <- traverse subtermValue classSuperClass
    name <- either (throwEvalError . FreeVariablesError) pure (freeVariable $ subterm classIdentifier)
    letrec' name $ \addr ->
      subtermValue classBody <* makeNamespace name addr super

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval (Module iden xs) = do
    name <- either (throwEvalError . FreeVariablesError) pure (freeVariable $ subterm iden)
    letrec' name $ \addr ->
      eval xs <* makeNamespace name addr Nothing

data LowPrecedenceBoolean a
  = LowAnd !a !a
  | LowOr !a !a
  deriving (Diffable, Eq, Foldable, Functor, GAlign, Generic1, Mergeable, Ord, Show, Traversable, FreeVariables1, Declarations1)

instance Evaluatable LowPrecedenceBoolean where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval = go . fmap subtermValue where
    go (LowAnd a b) = do
      cond <- a
      ifthenelse cond b (pure cond)
    go (LowOr a b) = do
      cond <- a
      ifthenelse cond (pure cond) b

instance Eq1 LowPrecedenceBoolean where liftEq = genericLiftEq
instance Ord1 LowPrecedenceBoolean where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceBoolean where liftShowsPrec = genericLiftShowsPrec
