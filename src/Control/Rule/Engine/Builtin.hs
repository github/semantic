{-# LANGUAGE RankNTypes, DuplicateRecordFields #-}

module Control.Rule.Engine.Builtin where

import Control.Rule
import Control.Monad.Reader
import Control.Monad.Reader
import Data.Reprinting.Token
import Data.Machine
import qualified Data.Machine as Machine
import Control.Monad.Effect (Eff, Member)
import qualified Control.Monad.Effect as Eff
import Control.Monad.Effect.State
import Debug.Trace (traceM)
import Data.Functor.Identity

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
  , wcurrent   :: from
  } deriving (Show, Eq)

contextuallyP :: (Member (State [Context]) effs)
              => PlanT
                 (Is Token)
                 (With [Context] Token)
                 (Eff effs)
                 ()
contextuallyP = do
  t <- await
  case t of
    TControl (Enter e) -> lift (modify' @[Context] (e :))
    TControl (Exit _)  -> lift (modify' @[Context] (drop 1))
    _                  -> traceM (show t)

  st <- lift get
  yield (With st t)

contextually :: (Member (State [Context]) effs)
             => Rule (Eff effs) Token (With [Context] Token)
contextually = Rule ["[builtin] contextually"] (repeatedly contextuallyP)

runContextually :: Foldable f
                => f from
                -> ProcessT (Eff '[State [Context]]) from to
                -> [to]
runContextually fs m
  = Eff.run
  . fmap snd
  . runState ([] :: [Context])
  . Machine.runT
  $ source fs ~> m

-- contextually :: Monad m => Rule m Token (With [Context] Token)
-- contextually = do
--   t <- await

--   fromPlan "[builtin] contextually" $ do
