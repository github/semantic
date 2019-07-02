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

import Control.Effect
import Control.Effect.Fail
import Control.Effect.Reader
import Control.Monad ((>=>))
import Data.Core as Core
import Data.File
import Data.Functor
import Data.Loc
import Data.Maybe (fromJust)
import Data.Name
import Data.Text (Text)
import GHC.Stack
import Prelude hiding (fail)

eval :: (Carrier sig m, Member Naming sig, Member (Reader Loc) sig, MonadFail m) => Analysis address value m -> (Core Name -> m value) -> Core Name -> m value
eval Analysis{..} eval = \case
  Var n -> lookupEnv' n >>= deref' n
  Core c -> case c of
    Let n -> alloc n >>= bind n >> unit
    a :>> b -> eval a >> eval b
    Lam b -> do
      n <- Gen <$> gensym "lam"
      abstract eval n (instantiate (const (pure n)) b)
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
          Core c -> case c of
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


prog1 :: File (Core Name)
prog1 = fromBody . lam foo $ block
  [ let' bar .= pure foo
  , Core.if' (pure bar)
    (Core.bool False)
    (Core.bool True)
  ]
  where (foo, bar) = (User "foo", User "bar")

prog2 :: File (Core Name)
prog2 = fromBody $ fileBody prog1 $$ Core.bool True

prog3 :: File (Core Name)
prog3 = fromBody $ lams [foo, bar, quux]
  (Core.if' (pure quux)
    (pure bar)
    (pure foo))
  where (foo, bar, quux) = (User "foo", User "bar", User "quux")

prog4 :: File (Core Name)
prog4 = fromBody
  $  let' foo .= Core.bool True
  <> Core.if' (pure foo)
    (Core.bool True)
    (Core.bool False)
  where foo = User "foo"

prog5 :: File (Core Name)
prog5 = fromBody $ block
  [ let' (User "mkPoint") .= lam (User "_x") (lam (User "_y") (block
    [ let' (User "x") .= pure (User "_x")
    , let' (User "y") .= pure (User "_y")]))
  , let' (User "point") .= pure (User "mkPoint") $$ Core.bool True $$ Core.bool False
  , pure (User "point") Core.... pure (User "x")
  , pure (User "point") Core.... pure (User "y") .= pure (User "point") Core.... pure (User "x")
  ]

prog6 :: [File (Core Name)]
prog6 =
  [ File (Loc "dep"  (locSpan (fromJust here))) $ block
    [ let' (User "dep") .= Core.frame
    , pure (User "dep") Core.... block
      [ let' (User "var") .= Core.bool True
      ]
    ]
  , File (Loc "main" (locSpan (fromJust here))) $ block
    [ load (Core.string "dep")
    , let' (User "thing") .= pure (User "dep") Core.... pure (User "var")
    ]
  ]

ruby :: File (Core Name)
ruby = fromBody . ann . block $
  [ ann (let' (User "Class") .= Core.frame)
  , ann (pure (User "Class") Core....
    (ann (let' (User "new") .= lam (User "self") (block
      [ ann (let' (User "instance") .= Core.frame)
      , ann (pure (User "instance") Core.... Core.edge Import (pure (User "self")))
      , ann (pure (User "instance") $$$ "initialize")
      ]))))

  , ann (let' (User "(Object)") .= Core.frame)
  , ann (pure (User "(Object)") Core.... ann (Core.edge Import (pure (User "Class"))))
  , ann (let' (User "Object") .= Core.frame)
  , ann (pure (User "Object") Core.... block
    [ ann (Core.edge Import (pure (User "(Object)")))
    , ann (let' (User "nil?") .= lam (User "_") false)
    , ann (let' (User "initialize") .= lam (User "self") (pure (User "self")))
    , ann (let' __semantic_truthy .= lam (User "_") (Core.bool True))
    ])

  , ann (pure (User "Class") Core.... Core.edge Import (pure (User "Object")))

  , ann (let' (User "(NilClass)") .= Core.frame)
  , ann (pure (User "(NilClass)") Core.... block
    [ ann (Core.edge Import (pure (User "Class")))
    , ann (Core.edge Import (pure (User "(Object)")))
    ])
  , ann (let' (User "NilClass") .= Core.frame)
  , ann (pure (User "NilClass") Core.... block
    [ ann (Core.edge Import (pure (User "(NilClass)")))
    , ann (Core.edge Import (pure (User "Object")))
    , ann (let' (User "nil?") .= lam (User "_") true)
    , ann (let' __semantic_truthy .= lam (User "_") (Core.bool False))
    ])

  , ann (let' (User "(TrueClass)") .= Core.frame)
  , ann (pure (User "(TrueClass)") Core.... block
    [ ann (Core.edge Import (pure (User "Class")))
    , ann (Core.edge Import (pure (User "(Object)")))
    ])
  , ann (let' (User "TrueClass") .= Core.frame)
  , ann (pure (User "TrueClass") Core.... block
    [ ann (Core.edge Import (pure (User "(TrueClass)")))
    , ann (Core.edge Import (pure (User "Object")))
    ])

  , ann (let' (User "(FalseClass)") .= Core.frame)
  , ann (pure (User "(FalseClass)") Core.... block
    [ ann (Core.edge Import (pure (User "Class")))
    , ann (Core.edge Import (pure (User "(Object)")))
    ])
  , ann (let' (User "FalseClass") .= Core.frame)
  , ann (pure (User "FalseClass") Core.... block
    [ ann (Core.edge Import (pure (User "(FalseClass)")))
    , ann (Core.edge Import (pure (User "Object")))
    , ann (let' __semantic_truthy .= lam (User "_") (Core.bool False))
    ])

  , ann (let' (User "nil")   .= pure (User "NilClass")   $$$ "new")
  , ann (let' (User "true")  .= pure (User "TrueClass")  $$$ "new")
  , ann (let' (User "false") .= pure (User "FalseClass") $$$ "new")

  , ann (let' (User "require") .= lam (User "path") (Core.load (pure (User "path"))))
  ]
  where -- _nil  = pure (User "nil")
        true  = pure (User "true")
        false = pure (User "false")
        self $$$ method = annWith callStack $ lam (User "_x") (pure (User "_x") Core.... pure (User method) $$ pure (User "_x")) $$ self

        __semantic_truthy = User "__semantic_truthy"


data Analysis address value m = Analysis
  { alloc       :: Name -> m address
  , bind        :: Name -> address -> m ()
  , lookupEnv   :: Name -> m (Maybe address)
  , deref       :: address -> m (Maybe value)
  , assign      :: address -> value -> m ()
  , abstract    :: (Core Name -> m value) -> Name -> Core Name -> m value
  , apply       :: (Core Name -> m value) -> value -> value -> m value
  , unit        :: m value
  , bool        :: Bool -> m value
  , asBool      :: value -> m Bool
  , string      :: Text -> m value
  , asString    :: value -> m Text
  , frame       :: m value
  , edge        :: Edge -> address -> m ()
  , (...)       :: forall a . address -> m a -> m a
  }
