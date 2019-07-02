{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DerivingVia, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, TypeApplications, TypeOperators, UndecidableInstances, AllowAmbiguousTypes #-}

module Language.Python.Prelude
  ( python
  ) where

import Data.File
import Data.Core (Core ((:=), (:.), (:$)), annWith)
import Data.Name
import qualified Data.Core as Core
import Control.Effect.Carrier
import Control.Effect.Writer
import Control.Effect.Sum
import Language.Preface
import Data.Maybe
import Control.Effect
import Data.Sequence (Seq ((:|>), Empty))
import qualified Data.Sequence as Seq
import Language.Preface
import GHC.Exts
import qualified Data.Loc as Loc
import GHC.Stack
import Debug.Trace

newtype PythonC m a = PythonC { runPythonC :: WriterC (Seq Core) m a }
  deriving newtype (Applicative, Functor, Monad)

emit :: (Member (Writer (Seq Core)) sig, Carrier sig m) => Core -> m ()
emit = tell @(Seq Core) . pure

block :: Seq Core -> Core
block Seq.Empty = Core.Unit
block (xs :|> Core.Unit) = Core.block (Seq.filter (/= Core.Unit) xs :|> Core.Unit)
block xs = Core.block (Seq.filter (/= Core.Unit) xs)

instance (Carrier sig m, Effect sig) => Carrier (Preface :+: sig) (PythonC m) where
  eff (R other) = PythonC (eff (R (handleCoercible other)))
  eff (L other) = PythonC $ case other of
    Metaclass stack name k ->
      emit (annWith stack (Core.Let (User name) := Core.Frame)) *> runPythonC k
    Klass stack name go k -> do
      emit (annWith stack (Core.Let (User name) := Core.Frame))
      (bod, res) <- censor @(Seq Core) (const (pure Core.Unit)) $ listen @(Seq Core) (runPythonC go)
      emit (annWith stack (Core.Var (User name) :. block bod))
      runPythonC $ k res
    Def stack name go k -> do
      (bod, res) <- censor @(Seq Core) (const (pure Core.Unit)) $ listen @(Seq Core) (runPythonC go)
      emit (annWith stack (Core.Let (User name)) := block bod)
      runPythonC $ k res
    Method stack name args go k -> do
      let arg = GHC.Exts.the args
      (bod, res) <- censor @(Seq Core) (const (pure Core.Unit)) $ listen @(Seq Core) (runPythonC go)
      emit (annWith stack (Core.Let (User name)) := Core.Lam (User arg) (block bod))
      runPythonC $ k res
    Open name go k -> do
      (bod, res) <- censor @(Seq Core) (const (pure Core.Unit)) $ listen @(Seq Core) (runPythonC go)
      emit (Core.Var (User name) :. block bod)
      runPythonC $ k res
    Inherit s k ->
      emit (Core.Edge Core.Import (Core.Var (User s))) *> runPythonC k

    Call l r args k -> do
      let arg = GHC.Exts.the args
      emit ((Core.Var (User l) :. Core.Var (User r)) :$ Core.Var (User arg)) *> runPythonC k

    Frame k -> emit Core.Frame *> runPythonC k

    New s k -> emit (Core.Var (User s) :$ Core.Var (User "__new__")) *> runPythonC k

    Var s k -> emit (Core.Var (User s)) *> runPythonC k

scribe :: (Monad m, HasCallStack) => PythonC m a -> m (File Core.Core, a)
scribe go = do
  (acc, res) <- runWriter $ runPythonC go
  let loc = fromMaybe (Loc.Loc "✏️" Loc.emptySpan) Loc.here
  pure (File loc (block acc), res)


prelude :: (Member Preface sig, Carrier sig m) => m ()
prelude = do
  metaclass "type"
  klass "object" $ do
    inherit "type"
    method "__new__" ["self"] $ do
      def "instance" frame
      open "instance" $ inherit "self"
      call "instance" "__init__" ["instance"]
    method "__init__" ["self"] $ var "self"

  klass "NoneType" $ inherit "object"

  def "None" $ new "NoneType"

  klass "bool" $ do
    inherit "object"
    method "__bool__" ["self"] $ var "self"

  def "True" $ new "bool"
  def "False" $ new "bool"

  open "object" $ do
    method "_bool__" ["self"] $ var "False"

python :: File Core
python = fst . run $ scribe prelude
