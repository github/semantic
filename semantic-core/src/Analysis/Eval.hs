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
    Let n -> alloc n >>= bind n >> unit
    a :>> b -> eval a >> eval b
    Lam (Ignored n) b -> abstract eval n (instantiate1 (pure n) b)
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
    Edge e a -> ref a >>= edge e >> unit
    Frame -> frame
    a :. b -> do
      a' <- ref a
      a' ... eval b
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
            Let n -> do
              addr <- alloc n
              addr <$ bind n addr
            If c t e -> do
              c' <- eval c >>= asBool
              if c' then ref t else ref e
            a :. b -> do
              a' <- ref a
              a' ... ref b
            Ann loc c -> local (const loc) (ref c)
            c -> invalidRef (show c)


prog1 :: File (Term Core User)
prog1 = fromBody . lam' foo $ block
  [ let' bar .= pure foo
  , Core.if' (pure bar)
    (Core.bool False)
    (Core.bool True)
  ]
  where (foo, bar) = ("foo", "bar")

prog2 :: File (Term Core User)
prog2 = fromBody $ fileBody prog1 $$ Core.bool True

prog3 :: File (Term Core User)
prog3 = fromBody $ lams' [foo, bar, quux]
  (Core.if' (pure quux)
    (pure bar)
    (pure foo))
  where (foo, bar, quux) = ("foo", "bar", "quux")

prog4 :: File (Term Core User)
prog4 = fromBody $ block
  [ let' foo .= Core.bool True
  , Core.if' (pure foo)
    (Core.bool True)
    (Core.bool False)
  ]
  where foo = "foo"

prog5 :: File (Term Core User)
prog5 = fromBody $ block
  [ let' "mkPoint" .= lam' "_x" (lam' "_y" (block
    [ let' "this" .= Core.frame
    , pure "this" Core.... let' "x" .= pure "_x"
    , pure "this" Core.... let' "y" .= pure "_y"
    , pure "this"
    ]))
  , let' "point" .= pure "mkPoint" $$ Core.bool True $$ Core.bool False
  , pure "point" Core.... pure "x"
  , pure "point" Core.... pure "y" .= pure "point" Core.... pure "x"
  ]

prog6 :: [File (Term Core User)]
prog6 =
  [ File (Loc "dep"  (locSpan (fromJust here))) $ block
    [ let' "dep" .= Core.frame
    , pure "dep" Core.... (let' "var" .= Core.bool True)
    ]
  , File (Loc "main" (locSpan (fromJust here))) $ block
    [ load (Core.string "dep")
    , let' "thing" .= pure "dep" Core.... pure "var"
    ]
  ]

ruby :: File (Term Core User)
ruby = fromBody . ann . block $
  [ ann (let' "Class" .= Core.frame)
  , ann (pure "Class" Core....
    (ann (let' "new" .= lam' "self" (block
      [ ann (let' "instance" .= Core.frame)
      , ann (pure "instance" Core.... Core.edge Import (pure "self"))
      , ann (pure "instance" $$$ "initialize")
      ]))))

  , ann (let' "(Object)" .= Core.frame)
  , ann (pure "(Object)" Core.... ann (Core.edge Import (pure ("Class"))))
  , ann (let' "Object" .= Core.frame)
  , ann (pure "Object" Core.... block
    [ ann (Core.edge Import (pure "(Object)"))
    , ann (let' "nil?" .= lam' "_" false)
    , ann (let' "initialize" .= lam' "self" (pure "self"))
    , ann (let' __semantic_truthy .= lam' "_" (Core.bool True))
    ])

  , ann (pure "Class" Core.... Core.edge Import (pure "Object"))

  , ann (let' "(NilClass)" .= Core.frame)
  , ann (pure "(NilClass)" Core.... block
    [ ann (Core.edge Import (pure "Class"))
    , ann (Core.edge Import (pure "(Object)"))
    ])
  , ann (let' "NilClass" .= Core.frame)
  , ann (pure "NilClass" Core.... block
    [ ann (Core.edge Import (pure "(NilClass)"))
    , ann (Core.edge Import (pure "Object"))
    , ann (let' "nil?" .= lam' "_" true)
    , ann (let' __semantic_truthy .= lam' "_" (Core.bool False))
    ])

  , ann (let' "(TrueClass)" .= Core.frame)
  , ann (pure "(TrueClass)" Core.... block
    [ ann (Core.edge Import (pure "Class"))
    , ann (Core.edge Import (pure "(Object)"))
    ])
  , ann (let' "TrueClass" .= Core.frame)
  , ann (pure "TrueClass" Core.... block
    [ ann (Core.edge Import (pure "(TrueClass)"))
    , ann (Core.edge Import (pure "Object"))
    ])

  , ann (let' "(FalseClass)" .= Core.frame)
  , ann (pure "(FalseClass)" Core.... block
    [ ann (Core.edge Import (pure "Class"))
    , ann (Core.edge Import (pure "(Object)"))
    ])
  , ann (let' "FalseClass" .= Core.frame)
  , ann (pure "FalseClass" Core.... block
    [ ann (Core.edge Import (pure "(FalseClass)"))
    , ann (Core.edge Import (pure "Object"))
    , ann (let' __semantic_truthy .= lam' "_" (Core.bool False))
    ])

  , ann (let' "nil"   .= pure "NilClass"   $$$ "new")
  , ann (let' "true"  .= pure "TrueClass"  $$$ "new")
  , ann (let' "false" .= pure "FalseClass" $$$ "new")

  , ann (let' "require" .= lam' "path" (Core.load (pure "path")))
  ]
  where -- _nil  = pure "nil"
        true  = pure "true"
        false = pure "false"
        self $$$ method = annWith callStack $ lam' "_x" (pure "_x" Core.... pure method $$ pure "_x") $$ self

        __semantic_truthy = "__semantic_truthy"


data Analysis address value m = Analysis
  { alloc       :: User -> m address
  , bind        :: User -> address -> m ()
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
  , frame       :: m value
  , edge        :: Edge -> address -> m ()
  , (...)       :: forall a . address -> m a -> m a
  }
