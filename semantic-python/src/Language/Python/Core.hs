{-# LANGUAGE DefaultSignatures, DeriveGeneric, FlexibleContexts, FlexibleInstances, RecordWildCards, StandaloneDeriving, TypeOperators #-}
module Language.Python.Core
( compile
) where

import Control.Monad.Fail
import Data.Core as Core
import GHC.Generics
import Prelude hiding (fail)
import TreeSitter.Python.AST as Py

class Compile t where
  -- FIXME: we should really try not to fail
  compile :: MonadFail m => t -> m Core
  default compile :: (MonadFail m, Show t) => t -> m Core
  compile = defaultCompile

defaultCompile :: (MonadFail m, Show t) => t -> m Core
defaultCompile t = fail $ "compilation unimplemented for " <> show t

instance (Compile l, Compile r) => Compile (Either l r) where
  compile = either compile compile

instance Compile Py.Module where
  compile (Module Nothing) = pure Unit
  compile (Module (Just statements)) = block <$> traverse compile statements

instance Compile Py.CompoundStatement where compile = compileSum

instance Compile Py.IfStatement where
  compile IfStatement{..} = If <$> compile condition <*> compile consequence <*> case alternative of
    Nothing -> pure Unit
    Just clauses -> foldr clause (pure Unit) clauses
    where clause (Left  (ElifClause{..}))  rest = If <$> compile condition <*> compile consequence <*> rest
          clause (Right (ElseClause body)) _    = compile body

instance Compile Py.Expression
instance Compile Py.Block
instance Compile Py.ClassDefinition
instance Compile Py.DecoratedDefinition
instance Compile Py.ForStatement
instance Compile Py.FunctionDefinition
instance Compile Py.TryStatement
instance Compile Py.WhileStatement
instance Compile Py.WithStatement

instance Compile Py.SimpleStatement


compileSum :: (Generic t, GCompileSum (Rep t), MonadFail m) => t -> m Core
compileSum = gcompileSum . from

class GCompileSum f where
  gcompileSum :: MonadFail m => f a -> m Core

instance GCompileSum f => GCompileSum (M1 D d f) where
  gcompileSum (M1 f) = gcompileSum f

instance (GCompileSum l, GCompileSum r) => GCompileSum (l :+: r) where
  gcompileSum (L1 l) = gcompileSum l
  gcompileSum (R1 r) = gcompileSum r

instance Compile t => GCompileSum (M1 C c (M1 S s (K1 R t))) where
  gcompileSum (M1 (M1 (K1 t))) = compile t


deriving instance Generic Py.CompoundStatement
