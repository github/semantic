{-# LANGUAGE DeriveAnyClass, DuplicateRecordFields, TupleSections #-}
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
resolveRubyName :: ( Member (Modules address) effects
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
resolveRubyPath :: ( Member (Modules address) effects
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
    func <- deref =<< maybe sel (flip evaluateInScopedEnv sel <=< subtermAddress) sendReceiver
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
    (importedEnv, v) <- doRequire path
    bindAll importedEnv
    rvalBox v -- Returns True if the file was loaded, False if it was already loaded. http://ruby-doc.org/core-2.5.0/Kernel.html#method-i-require

doRequire :: ( AbstractValue address value effects
             , Member (Modules address) effects
             )
          => M.ModulePath
          -> Evaluator address value effects (Environment address, value)
doRequire path = do
  result <- lookupModule path
  case result of
    Nothing       -> (, boolean True) . fst <$> load path
    Just (env, _) -> pure (env, boolean False)


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
          , Member (Modules address) effects
          , Member (Resumable ResolutionError) effects
          , Member Trace effects
          )
       => Text
       -> Bool
       -> Evaluator address value effects value
doLoad path shouldWrap = do
  path' <- resolveRubyPath path
  traceResolve path path'
  importedEnv <- fst <$> load path'
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
    super <- traverse subtermAddress classSuperClass
    name <- maybeM (throwEvalError NoNameError) (declaredName (subterm classIdentifier))
    rvalBox =<< letrec' name (\addr ->
      subtermValue classBody <* makeNamespace name addr super)

data Module a = Module { moduleIdentifier :: !a, moduleStatements :: ![a] }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Eq1 Module where liftEq = genericLiftEq
instance Ord1 Module where liftCompare = genericLiftCompare
instance Show1 Module where liftShowsPrec = genericLiftShowsPrec

instance Evaluatable Module where
  eval (Module iden xs) = do
    name <- maybeM (throwEvalError NoNameError) (declaredName (subterm iden))
    rvalBox =<< letrec' name (\addr ->
      value =<< (eval xs <* makeNamespace name addr Nothing))

data LowPrecedenceAnd a = LowPrecedenceAnd { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Evaluatable LowPrecedenceAnd where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval t = rvalBox =<< go (fmap subtermValue t) where
    go (LowPrecedenceAnd a b) = do
      cond <- a
      ifthenelse cond b (pure cond)

instance Eq1 LowPrecedenceAnd where liftEq = genericLiftEq
instance Ord1 LowPrecedenceAnd where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceAnd where liftShowsPrec = genericLiftShowsPrec

data LowPrecedenceOr a = LowPrecedenceOr { lhs :: a, rhs :: a }
  deriving (Declarations1, Diffable, Eq, Foldable, FreeVariables1, Functor, Generic1, Hashable1, Mergeable, Ord, Show, ToJSONFields1, Traversable, Named1, Message1)

instance Evaluatable LowPrecedenceOr where
  -- N.B. we have to use Monad rather than Applicative/Traversable on 'And' and 'Or' so that we don't evaluate both operands
  eval t = rvalBox =<< go (fmap subtermValue t) where
    go (LowPrecedenceOr a b) = do
      cond <- a
      ifthenelse cond (pure cond) b

instance Eq1 LowPrecedenceOr where liftEq = genericLiftEq
instance Ord1 LowPrecedenceOr where liftCompare = genericLiftCompare
instance Show1 LowPrecedenceOr where liftShowsPrec = genericLiftShowsPrec
