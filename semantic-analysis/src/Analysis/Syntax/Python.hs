{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
-- | This belongs in @semantic-python@ instead of @semantic-analysis@, but for the sake of expedience…
module Analysis.Syntax.Python
( -- * Syntax
  Term(..)
, subterms
, Python(..)
, pattern Noop''
, pattern Iff''
, pattern Bool''
, pattern String''
, pattern Throw''
, pattern Import''
  -- * Abstract interpretation
, eval0
, eval
) where

import           Analysis.Effect.Domain
import qualified Analysis.Effect.Statement as S
import           Analysis.Name
import           Analysis.Reference
import qualified Analysis.Syntax as T
import           Analysis.VM
import           Control.Effect.Labelled
import           Control.Effect.Reader
import           Data.Function (fix)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.Set as Set
import           Data.Text (Text)
import           Source.Span (Span)

-- Syntax

data Term
  = Var Name
  | Noop
  | Iff Term Term Term
  | Bool Bool
  | String Text
  | Throw Term
  | Let Name Term Term
  | Term :>> Term
  | Import (NonEmpty Text)
  | Function Name [Name] Term
  | Call Term [Term]
  | Locate Span Term
  deriving (Eq, Ord, Show)

infixl 1 :>>

subterms :: Term -> Set.Set Term
subterms t = Set.singleton t <> case t of
  Var _          -> mempty
  Noop           -> mempty
  Iff c t e      -> subterms c <> subterms t <> subterms e
  Bool _         -> mempty
  String _       -> mempty
  Throw t        -> subterms t
  Let _ v b      -> subterms v <> subterms b
  a :>> b        -> subterms a <> subterms b
  Import _       -> mempty
  Function _ _ b -> subterms b
  Call f as      -> subterms f <> foldMap subterms as
  Locate _ b     -> subterms b


data Python (arity :: T.Nat) where
  Noop' :: Python T.N0
  Iff' :: Python T.N3
  Bool' :: Bool -> Python T.N0
  String' :: Text -> Python T.N0
  Throw' :: Python T.N1
  (:>>>) :: Python T.N2
  Import' :: NonEmpty Text -> Python T.N0
  Function' :: Name -> [Name] -> Python T.N1
  Call' :: Python T.N2 -- ^ Second should be an @ANil'@ or @ACons'@.
  ANil' :: Python T.N0
  ACons' :: Python T.N2 -- ^ Second should be an @ANil'@ or @ACons'@.
  Locate' :: Span -> Python T.N1

infixl 1 :>>>

pattern Noop'' :: T.Term Python v
pattern Noop'' <- Noop' T.:$: T.Nil

pattern Iff'' :: T.Term Python v -> T.Term Python v -> T.Term Python v -> T.Term Python v
pattern Iff'' c t e <- Iff' T.:$: T.Cons c (T.Cons t (T.Cons e T.Nil))

pattern Bool'' :: Bool -> T.Term Python v
pattern Bool'' b <- Bool' b T.:$: T.Nil

pattern String'' :: Text -> T.Term Python v
pattern String'' t <- String' t T.:$: T.Nil

pattern Throw'' :: T.Term Python v -> T.Term Python v
pattern Throw'' e <- Throw' T.:$: T.Cons e T.Nil

pattern Import'' :: NonEmpty Text -> T.Term Python v
pattern Import'' i <- Import' i T.:$: T.Nil


-- Abstract interpretation

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m) => Term -> m val
eval0 = fix eval

eval
  :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m)
  => (Term -> m val)
  -> (Term -> m val)
eval eval = \case
  Var n     -> lookupEnv n >>= maybe (dvar n) fetch
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
