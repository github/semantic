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

eval :: (Carrier sig m, Member (Reader Loc) sig, MonadFail m) => Analysis address value m -> (Core -> m value) -> Core -> m value
eval Analysis{..} eval = \case
  Var n -> lookupEnv' n >>= deref' n
  Let n -> alloc n >>= bind n >> unit
  a :>> b -> eval a >> eval b
  Lam n b -> abstract eval n b
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
  Load p -> do
    path <- eval p >>= asString
    lookupEnv' (Path path) >>= deref' (Path path)
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


prog1 :: File Core
prog1 = fromBody $ Lam foo
  (   Let bar := Var foo
  :>> If (Var bar)
    (Bool False)
    (Bool True))
  where (foo, bar) = (User "foo", User "bar")

prog2 :: File Core
prog2 = fromBody $ fileBody prog1 :$ Bool True

prog3 :: File Core
prog3 = fromBody $ lams [foo, bar, quux]
  (If (Var quux)
    (Var bar)
    (Var foo))
  where (foo, bar, quux) = (User "foo", User "bar", User "quux")

prog4 :: File Core
prog4 = fromBody
  $   Let foo := Bool True
  :>> If (Var foo)
    (Bool True)
    (Bool False)
  where foo = User "foo"

prog5 :: File Core
prog5 = fromBody
  $   Let (User "mkPoint") := Lam (User "_x") (Lam (User "_y")
    (   Let (User "x") := Var (User "_x")
    :>> Let (User "y") := Var (User "_y")))
  :>> Let (User "point") := Var (User "mkPoint") :$ Bool True :$ Bool False
  :>> Var (User "point") :. Var (User "x")
  :>> Var (User "point") :. Var (User "y") := Var (User "point") :. Var (User "x")

prog6 :: [File Core]
prog6 =
  [ File (Loc "dep"  (locSpan (fromJust here))) $ block
    [ Let (Path "dep") := Frame
    , Var (Path "dep") :. block
      [ Let (User "var") := Bool True
      ]
    ]
  , File (Loc "main" (locSpan (fromJust here))) $ block
    [ Load (Var (Path "dep"))
    , Let (User "thing") := Var (Path "dep") :. Var (User "var")
    ]
  ]

ruby :: File Core
ruby = fromBody . ann . block $
  [ ann (Let (User "Class") := Frame)
  , ann (Var (User "Class") :.
    (ann (Let (User "new") := Lam (User "self") (block
      [ ann (Let (User "instance") := Frame)
      , ann (Var (User "instance") :. Edge Import (Var (User "self")))
      , ann (Var (User "instance") $$ "initialize")
      ]))))

  , ann (Let (User "(Object)") := Frame)
  , ann (Var (User "(Object)") :. ann (Edge Import (Var (User "Class"))))
  , ann (Let (User "Object") := Frame)
  , ann (Var (User "Object") :. block
    [ ann (Edge Import (Var (User "(Object)")))
    , ann (Let (User "nil?") := Lam (User "_") false)
    , ann (Let (User "initialize") := Lam (User "self") (Var (User "self")))
    , ann (Let __semantic_truthy := Lam (User "_") (Bool True))
    ])

  , ann (Var (User "Class") :. Edge Import (Var (User "Object")))

  , ann (Let (User "(NilClass)") := Frame)
  , ann (Var (User "(NilClass)") :. block
    [ ann (Edge Import (Var (User "Class")))
    , ann (Edge Import (Var (User "(Object)")))
    ])
  , ann (Let (User "NilClass") := Frame)
  , ann (Var (User "NilClass") :. block
    [ ann (Edge Import (Var (User "(NilClass)")))
    , ann (Edge Import (Var (User "Object")))
    , ann (Let (User "nil?") := Lam (User "_") true)
    , ann (Let __semantic_truthy := Lam (User "_") (Bool False))
    ])

  , ann (Let (User "(TrueClass)") := Frame)
  , ann (Var (User "(TrueClass)") :. block
    [ ann (Edge Import (Var (User "Class")))
    , ann (Edge Import (Var (User "(Object)")))
    ])
  , ann (Let (User "TrueClass") := Frame)
  , ann (Var (User "TrueClass") :. block
    [ ann (Edge Import (Var (User "(TrueClass)")))
    , ann (Edge Import (Var (User "Object")))
    ])

  , ann (Let (User "(FalseClass)") := Frame)
  , ann (Var (User "(FalseClass)") :. block
    [ ann (Edge Import (Var (User "Class")))
    , ann (Edge Import (Var (User "(Object)")))
    ])
  , ann (Let (User "FalseClass") := Frame)
  , ann (Var (User "FalseClass") :. block
    [ ann (Edge Import (Var (User "(FalseClass)")))
    , ann (Edge Import (Var (User "Object")))
    , ann (Let __semantic_truthy := Lam (User "_") (Bool False))
    ])

  , ann (Let (User "nil")   := Var (User "NilClass")   $$ "new")
  , ann (Let (User "true")  := Var (User "TrueClass")  $$ "new")
  , ann (Let (User "false") := Var (User "FalseClass") $$ "new")

  , ann (Let (User "require") := Lam (User "path") (Load (Var (User "path"))))
  ]
  where _nil  = Var (User "nil")
        true  = Var (User "true")
        false = Var (User "false")
        self $$ method = annWith callStack $ Lam (User "_x") (Var (User "_x") :. Var (User method) :$ Var (User "_x")) :$ self

        __semantic_truthy = User "__semantic_truthy"


data Analysis address value m = Analysis
  { alloc       :: Name -> m address
  , bind        :: Name -> address -> m ()
  , lookupEnv   :: Name -> m (Maybe address)
  , deref       :: address -> m (Maybe value)
  , assign      :: address -> value -> m ()
  , abstract    :: (Core -> m value) -> Name -> Core -> m value
  , apply       :: (Core -> m value) -> value -> value -> m value
  , unit        :: m value
  , bool        :: Bool -> m value
  , asBool      :: value -> m Bool
  , string      :: Text -> m value
  , asString    :: value -> m Text
  , frame       :: m value
  , edge        :: Edge -> address -> m ()
  , (...)       :: forall a . address -> m a -> m a
  }
