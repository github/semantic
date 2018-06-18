{-# LANGUAGE DeriveAnyClass #-}
module Language.Ruby.Syntax where

import           Control.Monad (unless)
import           Data.Abstract.Evaluatable
import qualified Data.Abstract.Module as M
import           Data.Abstract.Path
import           Data.JSON.Fields
import qualified Data.Language as Language
import qualified Data.Text as T
import           Diffing.Algorithm
import           Prologue
import           Proto3.Suite.Class
import           System.FilePath.Posix


-- TODO: Fully sort out ruby require/load mechanics
--
-- require "json"
resolveRubyName :: ( Member (Modules address value) effects
                   , Member (Resumable ResolutionError) effects
                   )
                => Text
                -> Evaluator address value effects M.ModulePath
resolveRubyName name = do
  let name' = cleanNameOrPath name
  let paths = [name' <.> "rb"]
  modulePath <- resolve paths
  maybeM (throwResumable $ NotFoundError name' paths Language.Ruby) modulePath

-- load "/root/src/file.rb"
resolveRubyPath :: ( Member (Modules address value) effects
                   , Member (Resumable ResolutionError) effects
                   )
                => Text
                -> Evaluator address value effects M.ModulePath
resolveRubyPath path = do
  let name' = cleanNameOrPath path
  modulePath <- resolve [name']
  maybeM (throwResumable $ NotFoundError name' [name'] Language.Ruby) modulePath

cleanNameOrPath :: Text -> String
cleanNameOrPath = T.unpack . dropRelativePrefix . stripQuotes

data Send a = Send { sendReceiver :: Maybe a, sendSelector :: Maybe a, sendArgs :: [a], sendBlock :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Send where liftEq = genericLiftEq
instance Ord1 Send where liftCompare = genericLiftCompare
instance Show1 Send where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Send where
  eval Send{..} = do
    let sel = case sendSelector of
          Just sel -> subtermAddress sel
          Nothing  -> variable (name "call")
    func <- deref =<< maybe sel (flip evaluateInScopedEnv sel . subtermValue) sendReceiver
    Rval <$> call func (map subtermAddress sendArgs) -- TODO pass through sendBlock

data Require a = Require { requireRelative :: Bool, requirePath :: !a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Require where liftEq = genericLiftEq
instance Ord1 Require where liftCompare = genericLiftCompare
instance Show1 Require where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Require where
  eval (Require _ x) = do
    name <- subtermValue x >>= asString
    path <- resolveRubyName name
    traceResolve name path
    (v, importedEnv) <- doRequire path
    bindAll importedEnv
    rvalBox v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

doRequire :: ( AbstractValue address value effects
             , Member (Modules address value) effects
             )
          => M.ModulePath
          -> Evaluator address value effects (value, Environment address)
doRequire path = do
  result <- join <$> lookupModule path
  case result of
    Nothing       -> (,) (boolean True) . maybe lowerBound snd <$> load path
    Just (_, env) -> pure (boolean False, env)


data Load a = Load { loadPath :: a, loadWrap :: Maybe a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Load where liftEq = genericLiftEq
instance Ord1 Load where liftCompare = genericLiftCompare
instance Show1 Load where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Load where
  eval (Load x Nothing) = do
    path <- subtermValue x >>= asString
    rvalBox =<< doLoad path False
  eval (Load x (Just wrap)) = do
    path <- subtermValue x >>= asString
    shouldWrap <- subtermValue wrap >>= asBool
    rvalBox =<< doLoad path shouldWrap

doLoad :: ( AbstractValue address value effects
          , Member (Env address) effects
          , Member (Modules address value) effects
          , Member (Resumable ResolutionError) effects
          , Member Trace effects
          )
       => Text
       -> Bool
       -> Evaluator address value effects value
doLoad path shouldWrap = do
  path' <- resolveRubyPath path
  traceResolve path path'
  importedEnv <- maybe lowerBound snd <$> load path'
  unless shouldWrap $ bindAll importedEnv
  pure (boolean Prelude.True) -- load always returns true. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-load

-- TODO: autoload

data Class a = Class { classIdentifier :: !a, classSuperClass :: !(Maybe a), classBody :: !a }
  deriving (Eq, Ord, Show, Foldable, Traversable, Functor, Generic1, Hashable1, Mergeable, FreeVariables1, Declarations1, ToJSONFields1, Named1, Message1)

instance Diffable Class where
  equivalentBySubterm = Just . classIdentifier

instance Eq1 Class where liftEq = genericLiftEq
instance Ord1 Class where liftCompare = genericLiftCompare
instance Show1 Class where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Class where
  eval Class{..} = do
    super <- traverse subtermValue classSuperClass
    name <- either (throwEvalError . FreeVariablesError) pure (freeVariable $ subterm classIdentifier)
    rvalBox =<< letrec' name (\addr ->
      subtermValue classBody <* makeNamespace name addr super)

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval (Module iden xs) = do
    name <- either (throwEvalError . FreeVariablesError) pure (freeVariable $ subterm iden)
    rvalBox =<< letrec' name (\addr ->
      value =<< (eval xs <* makeNamespace name addr Nothing))

data LowPrecedenceBoolean a
  = LowAnd !a !a
  | LowOr !a !a
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Evaluatable LowPrecedenceBoolean where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval t = rvalBox =<< go (fmap subtermValue t) where
    go (LowAnd a b) = do
      cond <- a
      ifthenelse cond b (pure cond)
    go (LowOr a b) = do
      cond <- a
      ifthenelse cond (pure cond) b

instance Eq1 LowPrecedenceBoolean where liftEq = genericLiftEq
instance Ord1 LowPrecedenceBoolean where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceBoolean where liftShowsPrec = genericLiftShowsPrec
