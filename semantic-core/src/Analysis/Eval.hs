{-# LANGUAGE FlexibleContexts, LambdaCase, OverloadedStrings, RankNTypes, RecordWildCards #-}
module Analysis.Eval
( eval
, prog1
, prog2
, prog3
, prog4
, prog5
, prog6
, ruby
, Analysis(..)
) where

import Control.Applicative (Alternative (..))
import Control.Effect.Fail
import Control.Effect.Reader
import Control.Monad ((>=>))
import Data.Core as Core
import Data.File
import Data.Functor
import Data.Loc
import Data.Maybe (fromJust, fromMaybe)
import Data.Name
import Data.Scope
import Data.Term
import Data.Text (Text)
import GHC.Stack
import Prelude hiding (fail)

eval :: ( Carrier sig m
        , Member (Reader Loc) sig
        , MonadFail m
        )
     => Analysis address value m
     -> (Term Core User -> m value)
     -> (Term Core User -> m value)
eval Analysis{..} eval = \case
  Var n -> lookupEnv' n >>= deref' n
  Term c -> case c of
    Rec (Named (Ignored n) b) -> do
      addr <- alloc n
      v <- bind n addr (eval (instantiate1 (pure n) b))
      v <$ assign addr v
    a :>> b -> eval a >> eval b
    Named (Ignored n) a :>>= b -> do
      a' <- eval a
      addr <- alloc n
      assign addr a'
      bind n addr (eval (instantiate1 (pure n) b))
    Lam (Named (Ignored n) b) -> abstract eval n (instantiate1 (pure n) b)
    f :$ a -> do
      f' <- eval f
      a' <- eval a
      apply eval f' a'
    Unit -> unit
    Bool b -> bool b
    If c t e -> do
      c' <- eval c >>= asBool
      if c' then eval t else eval e
    String s -> string s
    Load p -> eval p >>= asString >> unit -- FIXME: add a load command or something
    Record fields -> traverse (traverse eval) fields >>= record
    a :. b -> do
      a' <- ref a
      a' ... b >>= maybe (freeVariable (show b)) (deref' b)
    a := b -> do
      b' <- eval b
      addr <- ref a
      b' <$ assign addr b'
    Ann loc c -> local (const loc) (eval c)
  where freeVariable s = fail ("free variable: " <> s)
        uninitialized s = fail ("uninitialized variable: " <> s)
        invalidRef s = fail ("invalid ref: " <> s)

        lookupEnv' n = lookupEnv n >>= maybe (freeVariable (show n)) pure
        deref' n = deref >=> maybe (uninitialized (show n)) pure

        ref = \case
          Var n -> lookupEnv' n
          Term c -> case c of
            If c t e -> do
              c' <- eval c >>= asBool
              if c' then ref t else ref e
            a :. b -> do
              a' <- ref a
              a' ... b >>= maybe (freeVariable (show b)) pure
            Ann loc c -> local (const loc) (ref c)
            c -> invalidRef (show c)


prog1 :: File (Term Core User)
prog1 = fromBody $ lam (named' "foo")
  (    named' "bar" :<- pure "foo"
  >>>= Core.if' (pure "bar")
    (Core.bool False)
    (Core.bool True))

prog2 :: File (Term Core User)
prog2 = fromBody $ fileBody prog1 $$ Core.bool True

prog3 :: File (Term Core User)
prog3 = fromBody $ lams [named' "foo", named' "bar", named' "quux"]
  (Core.if' (pure "quux")
    (pure "bar")
    (pure "foo"))

prog4 :: File (Term Core User)
prog4 = fromBody
  (    named' "foo" :<- Core.bool True
  >>>= Core.if' (pure "foo")
    (Core.bool True)
    (Core.bool False))

prog5 :: File (Term Core User)
prog5 = fromBody $ ann (do'
  [ Just (named' "mkPoint") :<- lams [named' "_x", named' "_y"] (ann (Core.record
    [ ("x", ann (pure "_x"))
    , ("y", ann (pure "_y"))
    ]))
  , Just (named' "point") :<- ann (ann (ann (pure "mkPoint") $$ ann (Core.bool True)) $$ ann (Core.bool False))
  , Nothing :<- ann (ann (pure "point") Core.... "x")
  , Nothing :<- ann (ann (pure "point") Core.... "y") .= ann (ann (pure "point") Core.... "x")
  ])

prog6 :: [File (Term Core User)]
prog6 =
  [ File (Loc "dep"  (locSpan (fromJust here))) $ Core.record
    [ ("dep", Core.record [ ("var", Core.bool True) ]) ]
  , File (Loc "main" (locSpan (fromJust here))) $ do' (map (Nothing :<-)
    [ load (Core.string "dep")
    , Core.record [ ("thing", pure "dep" Core.... "var") ]
    ])
  ]

ruby :: File (Term Core User)
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


data Analysis address value m = Analysis
  { alloc       :: User -> m address
  , bind        :: forall a . User -> address -> m a -> m a
  , lookupEnv   :: User -> m (Maybe address)
  , deref       :: address -> m (Maybe value)
  , assign      :: address -> value -> m ()
  , abstract    :: (Term Core User -> m value) -> User -> Term Core User -> m value
  , apply       :: (Term Core User -> m value) -> value -> value -> m value
  , unit        :: m value
  , bool        :: Bool -> m value
  , asBool      :: value -> m Bool
  , string      :: Text -> m value
  , asString    :: value -> m Text
  , record      :: [(User, value)] -> m value
  , (...)       :: address -> User -> m (Maybe address)
  }
