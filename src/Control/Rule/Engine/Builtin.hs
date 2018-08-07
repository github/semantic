{-# LANGUAGE GeneralizedNewtypeDeriving, MultiWayIf, RankNTypes, ScopedTypeVariables, TupleSections, TypeFamilies #-}

module Control.Rule.Engine.Builtin where

import Prologue

import Control.Arrow
import           Control.Monad.Effect (Member)
import qualified Control.Monad.Effect as Eff
import           Control.Monad.Effect.State
import           Control.Monad.Trans
import           Control.Rule
import           Data.Machine
import qualified Data.Machine as Machine
import           Data.Reprinting.Token

trackingContexts :: Member (State [Context]) effs
                 => Rule effs Token (Token, [Context])
trackingContexts = fromPlan "[builtin] trackingContexts" $ do
  t <- await
  let go = yield =<< ((,) <$> pure t <*> lift get)
  case t of
    TControl (Enter e) -> go *> lift (modify' @[Context] (e :))
    TControl (Exit _)  -> go <* lift (modify' @[Context] (drop 1))
    _                  -> go

newtype Previous a = Previous (Maybe a)
  deriving (Eq, Show, Functor, Applicative, Lower)

remembering :: forall effs a . Rule effs a (Previous a, a)
remembering = fromPlanState "[builtin] remembering" $ \prev -> do
  x <- await
  yield (prev, x)
  pure (pure @Previous x)

fixingHashes :: Rule effs (Previous (Token, [Context]), (Token, [Context])) Token
fixingHashes = fromPlan "[builtin] fixing hashes" $ do
  (Previous last, (current, context)) <- await

  let isSeparator = fmap fst last == Just (TElement Separator)
  -- TODO: check for trailing chunks

  if
    | listToMaybe context /= Just Associative -> yield current
    | isNothing last -> yield current
    | isSeparator    -> yield current
    | otherwise      -> yield (TElement Separator) *> yield current

fixingPipeline :: (Member (State [Context]) effs) => Rule effs Token Token
fixingPipeline = trackingContexts >>> remembering >>> fixingHashes





















runContextually :: (Foldable f, effs ~ '[State [Context], State (Previous from)])
                => f from
                -> Rule effs from to
                -> [to]
runContextually fs r
  = Eff.run
  . fmap snd
  . runState (lowerBound @(Previous _))
  . fmap snd
  . runState ([] :: [Context])
  . Machine.runT
  $ source fs ~> machine r

{-

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

-- contextually :: Monad m => Rule m Token (With [Context] Token)
-- contextually = do
--   t <- await

--   fromPlan "[builtin] contextually" $ do
-}
