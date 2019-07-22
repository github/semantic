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

import Control.Effect.Fail
import Control.Effect.Reader
import Control.Monad ((>=>))
import Data.Core as Core
import Data.File
import Data.Functor
import Data.Loc
import Data.Maybe (fromJust)
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
prog5 = fromBody $ binds
  [ named' "mkPoint" :<- lams [named' "_x", named' "_y"] (Core.record
    [ ("x", pure "_x")
    , ("y", pure "_y")
    ])
  , named' "point" :<- pure "mkPoint" $$ Core.bool True $$ Core.bool False
  ]
  (   pure "point" Core.... "x"
  >>> pure "point" Core.... "y" .= pure "point" Core.... "x")

prog6 :: [File (Term Core User)]
prog6 =
  [ File (Loc "dep"  (locSpan (fromJust here))) $ Core.record
    [ ("dep", Core.record [ ("var", Core.bool True) ]) ]
  , File (Loc "main" (locSpan (fromJust here))) $ block
    [ load (Core.string "dep")
    , Core.record [ ("thing", pure "dep" Core.... "var") ]
    ]
  ]

ruby :: File (Term Core User)
ruby = fromBody $ ann (rec (named' __semantic_global) (Core.record
  [ ("Class", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "Object")
    , ("new", lam (named' "self")
      (    named' "instance" :<- Core.record [ (__semantic_super, pure "self") ]
      >>>= pure "instance" $$$ "initialize"))
    ]))

  , ("(Object)", ann (Core.record [ (__semantic_super, pure __semantic_global ... "Class") ]))
  , ("Object", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "(Object)")
    , ("nil?", lam (named' "_") (pure __semantic_global ... "false"))
    , ("initialize", lam (named' "self") (pure "self"))
    , (__semantic_truthy, lam (named' "_") (Core.bool True))
    ]))

  , ("(NilClass)", ann (Core.record
    -- FIXME: what should we do about multiple import edges like this
    [ (__semantic_super, pure __semantic_global ... "Class")
    , (__semantic_super, pure __semantic_global ... "(Object)")
    ]))
  , ("NilClass", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "(NilClass)")
    , (__semantic_super, pure __semantic_global ... "Object")
    , ("nil?", lam (named' "_") (pure __semantic_global ... "true"))
    , (__semantic_truthy, lam (named' "_") (Core.bool False))
    ]))

  , ("(TrueClass)", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "Class")
    , (__semantic_super, pure __semantic_global ... "(Object)")
    ]))
  , ("TrueClass", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "(TrueClass)")
    , (__semantic_super, pure __semantic_global ... "Object")
    ]))

  , ("(FalseClass)", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "Class")
    , (__semantic_super, pure __semantic_global ... "(Object)")
    ]))
  , ("FalseClass", ann (Core.record
    [ (__semantic_super, pure __semantic_global ... "(FalseClass)")
    , (__semantic_super, pure __semantic_global ... "Object")
    , (__semantic_truthy, lam (named' "_") (Core.bool False))
    ]))

  , ("nil"  , pure __semantic_global ... "NilClass"   $$$ "new")
  , ("true" , pure __semantic_global ... "TrueClass"  $$$ "new")
  , ("false", pure __semantic_global ... "FalseClass" $$$ "new")

  , ("require", lam (named' "path") (Core.load (pure "path")))
  ]))
  where self $$$ method = annWith callStack $ named' "_x" :<- self >>>= pure "_x" ... method $$ pure "_x"
        record ... field = annWith callStack $ record Core.... field

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
