{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Core.Eval
( eval
, prog1
, prog2
, prog3
, prog4
, prog5
, prog6
, ruby
) where

import qualified Analysis.Effect.Domain as A
import           Analysis.Effect.Env as A
import           Analysis.Effect.Heap as A
import           Analysis.File
import           Control.Algebra
import           Control.Applicative (Alternative (..))
import           Control.Effect.Fail
import           Control.Effect.Reader
import           Control.Monad ((>=>))
import           Core.Core as Core
import           Core.Name
import           Data.Functor
import           Data.Maybe (fromMaybe, isJust)
import           GHC.Stack
import           Prelude hiding (fail)
import           Source.Span
import           Syntax.Scope
import qualified Syntax.Term as Term
import qualified System.Path as Path

type Term = Term.Term (Ann Span :+: Core)

eval :: forall address value m sig
     .  ( Has (A.Domain Term address value) sig m
        , Has (Env address) sig m
        , Has (Heap address value) sig m
        , Has (Reader Span) sig m
        , MonadFail m
        , Semigroup value
        , Show address
        )
     => (Term address -> m value)
     -> (Term address -> m value)
eval eval = \case
  Term.Var n -> deref' n n
  Term.Alg (R c) -> case c of
    Rec (Named n b) -> do
      addr <- A.alloc @address n
      v <- A.bind n addr (eval (instantiate1 (pure addr) b))
      v <$ A.assign addr v
    -- NB: Combining the results of the evaluations allows us to model effects in abstract domains. This in turn means that we can define an abstract domain modelling the types-and-effects of computations by means of a 'Semigroup' instance which takes the type of its second operand and the union of both operands’ effects.
    --
    -- It’s also worth noting that we use a semigroup instead of a semilattice because the lattice structure of our abstract domains is instead modelled by nondeterminism effects used by some of them.
    a :>> b -> (<>) <$> eval a <*> eval b
    Named n a :>>= b -> do
      a' <- eval a
      addr <- A.alloc @address n
      A.assign addr a'
      A.bind n addr ((a' <>) <$> eval (instantiate1 (pure addr) b))
    Lam b -> A.lam b
    f :$ a -> do
      Named n b <- eval f >>= A.asLam
      a' <- eval a
      addr <- A.alloc @address n
      A.assign addr a'
      A.bind n addr (eval (instantiate1 (pure addr) b))
    If c t e -> do
      c' <- eval c >>= A.asBool
      if c' then eval t else eval e
    Load p -> eval p >>= A.asString >> A.unit -- FIXME: add a load command or something
    Unit     -> A.unit
    Bool b   -> A.bool b
    String s -> A.string s
    Record fields -> A.record fields
    a :. b -> do
      a' <- eval a >>= A.asRecord
      maybe (freeVariable (show b)) eval (lookup b a')
    a :? b -> do
      a' <- eval a >>= A.asRecord @Term @address
      A.bool (isJust (lookup b a'))

    a := b -> do
      b' <- eval b
      addr <- ref a
      b' <$ A.assign addr b'
  Term.Alg (L (Ann span c)) -> local (const span) (eval c)
  where freeVariable s = fail ("free variable: " <> s)
        uninitialized s = fail ("uninitialized variable: " <> s)
        invalidRef s = fail ("invalid ref: " <> s)

        deref' n = A.deref @address >=> maybe (uninitialized (show n)) pure

        ref = \case
          Term.Var n -> pure n
          Term.Alg (R c) -> case c of
            If c t e -> do
              c' <- eval c >>= A.asBool
              if c' then ref t else ref e
            a :. b -> do
              a' <- eval a >>= A.asRecord
              maybe (freeVariable (show b)) ref (lookup b a')
            c -> invalidRef (show c)
          Term.Alg (L (Ann span c)) -> local (const span) (ref c)


prog1 :: Has Core sig t => File (t Name)
prog1 = fromBody $ Core.lam (named' "foo")
  (    named' "bar" :<- pure "foo"
  >>>= Core.if' (pure "bar")
    (Core.bool False)
    (Core.bool True))

prog2 :: Has Core sig t => File (t Name)
prog2 = fromBody $ fileBody prog1 $$ Core.bool True

prog3 :: Has Core sig t => File (t Name)
prog3 = fromBody $ lams [named' "foo", named' "bar", named' "quux"]
  (Core.if' (pure "quux")
    (pure "bar")
    (pure "foo"))

prog4 :: Has Core sig t => File (t Name)
prog4 = fromBody
  (    named' "foo" :<- Core.bool True
  >>>= Core.if' (pure "foo")
    (Core.bool True)
    (Core.bool False))

prog5 :: (Has (Ann Span) sig t, Has Core sig t) => File (t Name)
prog5 = fromBody $ ann (do'
  [ Just (named' "mkPoint") :<- lams [named' "_x", named' "_y"] (ann (Core.record
    [ ("x", ann (pure "_x"))
    , ("y", ann (pure "_y"))
    ]))
  , Just (named' "point") :<- ann (ann (ann (pure "mkPoint") $$ ann (Core.bool True)) $$ ann (Core.bool False))
  , Nothing :<- ann (ann (pure "point") Core.... "x")
  , Nothing :<- ann (ann (pure "point") Core.... "y") .= ann (ann (pure "point") Core.... "x")
  ])

prog6 :: Has Core sig t => [File (t Name)]
prog6 =
  [ (fromBody (Core.record
    [ ("dep", Core.record [ ("var", Core.bool True) ]) ]))
    { filePath = Path.absRel "dep"}
  , (fromBody (do' (map (Nothing :<-)
    [ load (Core.string "dep")
    , Core.record [ ("thing", pure "dep" Core.... "var") ]
    ])))
    { filePath = Path.absRel "main" }
  ]

ruby :: (Has (Ann Span) sig t, Has Core sig t) => File (t Name)
ruby = fromBody $ annWith callStack (rec (named' __semantic_global) (do' statements))
  where statements =
          [ Just "Class" :<- record
            [ (__semantic_super, Core.record [])
            , ("new", lam "self"
              (    "instance" :<- record [ (__semantic_super, var "self") ]
              >>>= var "instance" $$$ "initialize"))
            ]

          , Just "(Object)" :<- record [ (__semantic_super, var "Class") ]
          , Just "Object" :<- record
            [ (__semantic_super, var "(Object)")
            , ("nil?", lam "_" (var __semantic_global ... "false"))
            , ("initialize", lam "self" (var "self"))
            , (__semantic_truthy, lam "_" (bool True))
            ]

          , Just "(NilClass)" :<- record
            -- FIXME: what should we do about multiple import edges like this
            [ (__semantic_super, var "Class")
            , (__semantic_super, var "(Object)")
            ]
          , Just "NilClass" :<- record
            [ (__semantic_super, var "(NilClass)")
            , (__semantic_super, var "Object")
            , ("nil?", lam "_" (var __semantic_global ... "true"))
            , (__semantic_truthy, lam "_" (bool False))
            ]

          , Just "(TrueClass)" :<- record
            [ (__semantic_super, var "Class")
            , (__semantic_super, var "(Object)")
            ]
          , Just "TrueClass" :<- record
            [ (__semantic_super, var "(TrueClass)")
            , (__semantic_super, var "Object")
            ]

          , Just "(FalseClass)" :<- record
            [ (__semantic_super, var "Class")
            , (__semantic_super, var "(Object)")
            ]
          , Just "FalseClass" :<- record
            [ (__semantic_super, var "(FalseClass)")
            , (__semantic_super, var "Object")
            , (__semantic_truthy, lam "_" (bool False))
            ]

          , Just "nil"   :<- var "NilClass"   $$$ "new"
          , Just "true"  :<- var "TrueClass"  $$$ "new"
          , Just "false" :<- var "FalseClass" $$$ "new"

          , Just "require" :<- lam "path" (Core.load (var "path"))

          , Nothing :<- var "Class" ... __semantic_super .= var "Object"
          , Nothing :<- record (statements >>= \ (v :<- _) -> maybe [] (\ v -> [(v, var v)]) v)
          ]
        self $$$ method = annWith callStack ("_x" :<- self >>>= var "_x" ... method $$ var "_x")
        record ... field = annWith callStack (record Core.... field)
        record bindings = annWith callStack (Core.record bindings)
        var x = annWith callStack (pure x)
        lam v b = annWith callStack (Core.lam (named' v) b)
        a >>> b = annWith callStack (a Core.>>> b)
        infixr 1 >>>
        v :<- a >>>= b = annWith callStack (named' v :<- a Core.>>>= b)
        infixr 1 >>>=
        do' bindings = fromMaybe Core.unit (foldr bind Nothing bindings)
          where bind (n :<- a) v = maybe (a >>>) ((>>>=) . (:<- a)) n <$> v <|> Just a
        bool b = annWith callStack (Core.bool b)
        a .= b = annWith callStack (a Core..= b)

        __semantic_global = "__semantic_global"
        __semantic_super  = "__semantic_super"
        __semantic_truthy = "__semantic_truthy"
