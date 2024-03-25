{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
-- | This belongs in @semantic-python@ instead of @semantic-analysis@, but for the sake of expedience…
module Analysis.Syntax.Python
( -- * Syntax
  Term
, Python(..)
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
import           Data.List.NonEmpty (NonEmpty)
import           Data.Text (Text)
import           Source.Span (Span)

-- Syntax

type Term = T.Term Python Name

data Python t
  = Noop
  | Iff t t t
  | Bool Bool
  | String Text
  | Throw t
  | Let Name t t
  | t :>> t
  | Import (NonEmpty Text)
  | Function Name [Name] t
  | Call t [t]
  | Locate Span t
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixl 1 :>>


-- Abstract interpretation

eval0 :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m) => Term -> m val
eval0 = fix eval

eval
  :: (Has (Env addr) sig m, HasLabelled Store (Store addr val) sig m, Has (Dom val) sig m, Has (Reader Reference) sig m, Has S.Statement sig m)
  => (Term -> m val)
  -> (Term -> m val)
eval eval = \case
  T.Var n  -> lookupEnv n >>= maybe (dvar n) fetch
  T.Term s -> case s of
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
