{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- | This belongs in @semantic-python@ instead of @semantic-analysis@, but for the sake of expedience…
module Analysis.Syntax.Python
( -- * Syntax
  Term
, Python(..)
, pattern Noop
, pattern Iff
, pattern Bool
, pattern String
, pattern Throw
, pattern Let
, pattern (:>>)
, pattern Import
, pattern Function
, pattern Call
, pattern Locate
  -- * Abstract interpretation
, eval0
, eval
) where

import           Analysis.Effect.Domain hiding ((:>>>))
import qualified Analysis.Effect.Statement as S
import           Analysis.Name
import           Analysis.Reference
import qualified Analysis.Syntax as T
import           Analysis.VM
import           Control.Effect.Labelled
import           Control.Effect.Reader
import           Data.Function (fix)
import           Data.Functor.Classes (Eq1 (..), Ord1 (..))
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Source.Span (Span)

-- Syntax

type Term = T.Term Python Name

data Python arity where
  Noop' :: Python T.N0
  Iff' :: Python T.N3
  Bool' :: Bool -> Python T.N0
  String' :: Text -> Python T.N0
  Throw' :: Python T.N1
  Let' :: Name -> Python T.N2
  (:>>>) :: Python T.N2
  Import' :: NonEmpty Text -> Python T.N0
  Function' :: Name -> [Name] -> Python T.N1
  Call' :: Python T.N2 -- ^ Second should be an @ANil'@ or @ACons'@.
  ANil' :: Python T.N0
  ACons' :: Python T.N2 -- ^ Second should be an @ANil'@ or @ACons'@.
  Locate' :: Span -> Python T.N1

infixl 1 :>>>

pattern Noop :: T.Term Python v
pattern Noop = Noop' T.:$: T.Nil

pattern Iff :: T.Term Python v -> T.Term Python v -> T.Term Python v -> T.Term Python v
pattern Iff c t e = Iff' T.:$: T.Cons c (T.Cons t (T.Cons e T.Nil))

pattern Bool :: Bool -> T.Term Python v
pattern Bool b = Bool' b T.:$: T.Nil

pattern String :: Text -> T.Term Python v
pattern String t = String' t T.:$: T.Nil

pattern Throw :: T.Term Python v -> T.Term Python v
pattern Throw e = Throw' T.:$: T.Cons e T.Nil

pattern Let :: Name -> T.Term Python v -> T.Term Python v -> T.Term Python v
pattern Let n v b = Let' n T.:$: T.Cons v (T.Cons b T.Nil)

pattern (:>>) :: T.Term Python v -> T.Term Python v -> T.Term Python v
pattern s :>> t = (:>>>) T.:$: T.Cons s (T.Cons t T.Nil)

infixl 1 :>>

pattern Import :: NonEmpty Text -> T.Term Python v
pattern Import i = Import' i T.:$: T.Nil

pattern Function :: Name -> [Name] -> T.Term Python v -> T.Term Python v
pattern Function n as b = Function' n as T.:$: T.Cons b T.Nil

pattern Call
  :: T.Term Python v
  -> [T.Term Python v]
  -> T.Term Python v
pattern Call f as <- Call' T.:$: T.Cons f (T.Cons (fromArgs -> as) T.Nil)
  where Call f as = Call' T.:$: T.Cons f (T.Cons (foldr ACons ANil as) T.Nil)

fromArgs :: T.Term Python v -> [T.Term Python v]
fromArgs = \case
  ANil -> []
  ACons a as -> a:fromArgs as
  _ -> fail "unexpected constructor in spine of argument list"

pattern ANil :: T.Term Python v
pattern ANil = ANil' T.:$: T.Nil

pattern ACons :: T.Term Python v -> T.Term Python v -> T.Term Python v
pattern ACons a as = ACons' T.:$: T.Cons a (T.Cons as T.Nil)

pattern Locate :: Span -> T.Term Python v -> T.Term Python v
pattern Locate s t = Locate' s T.:$: T.Cons t T.Nil

{-# COMPLETE Noop, Iff, Bool, String, Throw, Let, (:>>), Import, Function, Call, Locate #-}


instance Eq1 Python where
  liftEq _ a b = case (a, b) of
    (Noop', Noop')                       -> True
    (Iff', Iff')                         -> True
    (Bool' b1, Bool' b2)                 -> b1 == b2
    (String' s1, String' s2)             -> s1 == s2
    (Throw', Throw')                     -> True
    (Let' n1, Let' n2)                   -> n1 == n2
    ((:>>>), (:>>>))                     -> True
    (Import' i1, Import' i2)             -> i1 == i2
    (Function' n1 as1, Function' n2 as2) -> n1 == n2 && as1 == as2
    (Call', Call')                       -> True
    (ANil', ANil')                       -> True
    (ACons', ACons')                     -> True
    (Locate' s1, Locate' s2)             -> s1 == s2
    _                                    -> False

instance Ord1 Python where
  liftCompare _ a b = case (a, b) of
    (Noop', Noop')                       -> EQ
    (Noop', _)                           -> LT
    (Iff', Iff')                         -> EQ
    (Iff', _)                            -> LT
    (Bool' b1,  Bool' b2)                -> compare b1 b2
    (Bool' _,  _)                        -> LT
    (String' s1, String' s2)             -> compare s1 s2
    (String' _, _)                       -> LT
    (Throw', Throw')                     -> EQ
    (Throw', _)                          -> LT
    (Let' n1, Let' n2)                   -> compare n1 n2
    (Let' _, _)                          -> LT
    ((:>>>), (:>>>))                     -> EQ
    ((:>>>), _)                          -> LT
    (Import' i1, Import' i2)             -> compare i1 i2
    (Import' _, _)                       -> LT
    (Function' n1 as1, Function' n2 as2) -> compare n1 n2 <> compare as1 as2
    (Function' _ _, _)                   -> LT
    (Call', Call')                       -> EQ
    (Call', _)                           -> LT
    (ANil', ANil')                       -> EQ
    (ANil', _)                           -> LT
    (ACons', ACons')                     -> EQ
    (ACons', _)                          -> LT
    (Locate' s1, Locate' s2)             -> compare s1 s2
    (Locate' _, _)                       -> LT


-- Abstract interpretation

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m) => Term -> m val
eval0 = fix eval

eval
  :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m)
  => (Term -> m val)
  -> (Term -> m val)
eval eval = \case
  T.Var n   -> lookupEnv n >>= maybe (dvar n) fetch
  Noop      -> dunit
  Iff c t e -> do
    c' <- eval c
    dif c' (eval t) (eval e)
  Bool b    -> dbool b
  String s  -> dstring s
  Throw e   -> eval e >>= ddie
  Let n v b -> do
    v' <- eval v
    let' n v' (eval b)
  t :>> u   -> do
    t' <- eval t
    u' <- eval u
    t' >>> u'
  Import ns -> S.simport ns >> dunit
  Function n ps b -> letrec n (dabs ps (foldr (\ (p, a) m -> let' p a m) (eval b) . zip ps))
  Call f as -> do
    f' <- eval f
    as' <- traverse eval as
    dapp f' as'
  Locate s t -> local (setSpan s) (eval t)
  where
  setSpan s r = r{ refSpan = s }
