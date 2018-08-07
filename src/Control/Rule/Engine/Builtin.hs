{-# LANGUAGE RankNTypes, DuplicateRecordFields #-}

module Control.Rule.Engine.Builtin where

import Control.Rule
import Control.Monad.Reader
import Data.Reprinting.Token
import Control.Monad.State
import Data.Machine
import Debug.Trace (traceM)

justs :: Monad m => Rule m (Maybe it) it
justs = fromPlan "[builtin] justs" $
  await >>= maybe (pure ()) yield

data Trail from to = Trail
  { tcurrent  :: ~from
  , previous :: Maybe to
  }

remembering :: Monad m => (Trail from to -> to) -> Rule m from to
remembering f = Rule ["[builtin] remembering"] pipeline where
  pipeline = fold go initial ~> repeatedly filt
  initial  = Trail (error "invalid Trail access") Nothing
  filt = await @Is >>= maybeYield . previous
  go acc item =
    let
      stepped = acc { tcurrent = item }
      result  = f acc
    in stepped { previous = Just result }

data With ann from = With
  { additional :: ann
  , wcurrent   :: ~from
  } deriving (Show, Eq)

contextuallyP :: Monad m => PlanT (Is Token) (With [Context] Token) (StateT [Context] m) ()
contextuallyP = do
  t <- await
  case t of
    TControl (Enter e) -> modify' (e :)
    TControl (Exit _)  -> modify' (drop 1)
    _                  -> traceM (show t)

  st <- lift get
  yield (With st t)

contextually :: Monad m => Rule (StateT [Context] m) Token (With [Context] Token)
contextually = Rule ["[builtin] contextually"] (repeatedly contextuallyP)

-- contextually :: Monad m => Rule m Token (With [Context] Token)
-- contextually = do
--   t <- await

--   fromPlan "[builtin] contextually" $ do
