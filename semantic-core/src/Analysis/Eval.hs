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
  Let n -> alloc n >>= bind n >> unit
  a :>> b -> eval a >> eval b
  Lam b -> do
    n <- Gen <$> gensym "lam"
    abstract eval n (instantiate (pure n) b)
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
  [ Let bar := pure foo
  , If (pure bar)
    (Bool False)
    (Bool True)
  ]
  where (foo, bar) = (User "foo", User "bar")

prog2 :: File (Core Name)
prog2 = fromBody $ fileBody prog1 :$ Bool True

prog3 :: File (Core Name)
prog3 = fromBody $ lams [foo, bar, quux]
  (If (pure quux)
    (pure bar)
    (pure foo))
  where (foo, bar, quux) = (User "foo", User "bar", User "quux")

prog4 :: File (Core Name)
prog4 = fromBody
  $  Let foo := Bool True
  <> If (pure foo)
    (Bool True)
    (Bool False)
  where foo = User "foo"

prog5 :: File (Core Name)
prog5 = fromBody $ block
  [ Let (User "mkPoint") := lam (User "_x") (lam (User "_y") (block
    [ Let (User "x") := pure (User "_x")
    , Let (User "y") := pure (User "_y")]))
  , Let (User "point") := pure (User "mkPoint") :$ Bool True :$ Bool False
  , pure (User "point") :. pure (User "x")
  , pure (User "point") :. pure (User "y") := pure (User "point") :. pure (User "x")
  ]

prog6 :: [File (Core Name)]
prog6 =
  [ File (Loc "dep"  (locSpan (fromJust here))) $ block
    [ Let (User "dep") := Frame
    , pure (User "dep") :. block
      [ Let (User "var") := Bool True
      ]
    ]
  , File (Loc "main" (locSpan (fromJust here))) $ block
    [ Load (String "dep")
    , Let (User "thing") := pure (User "dep") :. pure (User "var")
    ]
  ]

ruby :: File (Core Name)
ruby = fromBody . ann . block $
  [ ann (Let (User "Class") := Frame)
  , ann (pure (User "Class") :.
    (ann (Let (User "new") := lam (User "self") (block
      [ ann (Let (User "instance") := Frame)
      , ann (pure (User "instance") :. Edge Import (pure (User "self")))
      , ann (pure (User "instance") $$ "initialize")
      ]))))

  , ann (Let (User "(Object)") := Frame)
  , ann (pure (User "(Object)") :. ann (Edge Import (pure (User "Class"))))
  , ann (Let (User "Object") := Frame)
  , ann (pure (User "Object") :. block
    [ ann (Edge Import (pure (User "(Object)")))
    , ann (Let (User "nil?") := lam (User "_") false)
    , ann (Let (User "initialize") := lam (User "self") (pure (User "self")))
    , ann (Let __semantic_truthy := lam (User "_") (Bool True))
    ])

  , ann (pure (User "Class") :. Edge Import (pure (User "Object")))

  , ann (Let (User "(NilClass)") := Frame)
  , ann (pure (User "(NilClass)") :. block
    [ ann (Edge Import (pure (User "Class")))
    , ann (Edge Import (pure (User "(Object)")))
    ])
  , ann (Let (User "NilClass") := Frame)
  , ann (pure (User "NilClass") :. block
    [ ann (Edge Import (pure (User "(NilClass)")))
    , ann (Edge Import (pure (User "Object")))
    , ann (Let (User "nil?") := lam (User "_") true)
    , ann (Let __semantic_truthy := lam (User "_") (Bool False))
    ])

  , ann (Let (User "(TrueClass)") := Frame)
  , ann (pure (User "(TrueClass)") :. block
    [ ann (Edge Import (pure (User "Class")))
    , ann (Edge Import (pure (User "(Object)")))
    ])
  , ann (Let (User "TrueClass") := Frame)
  , ann (pure (User "TrueClass") :. block
    [ ann (Edge Import (pure (User "(TrueClass)")))
    , ann (Edge Import (pure (User "Object")))
    ])

  , ann (Let (User "(FalseClass)") := Frame)
  , ann (pure (User "(FalseClass)") :. block
    [ ann (Edge Import (pure (User "Class")))
    , ann (Edge Import (pure (User "(Object)")))
    ])
  , ann (Let (User "FalseClass") := Frame)
  , ann (pure (User "FalseClass") :. block
    [ ann (Edge Import (pure (User "(FalseClass)")))
    , ann (Edge Import (pure (User "Object")))
    , ann (Let __semantic_truthy := lam (User "_") (Bool False))
    ])

  , ann (Let (User "nil")   := pure (User "NilClass")   $$ "new")
  , ann (Let (User "true")  := pure (User "TrueClass")  $$ "new")
  , ann (Let (User "false") := pure (User "FalseClass") $$ "new")

  , ann (Let (User "require") := lam (User "path") (Load (pure (User "path"))))
  ]
  where -- _nil  = pure (User "nil")
        true  = pure (User "true")
        false = pure (User "false")
        self $$ method = annWith callStack $ lam (User "_x") (pure (User "_x") :. pure (User method) :$ pure (User "_x")) :$ self

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
