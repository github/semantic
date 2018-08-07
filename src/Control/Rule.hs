{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Control.Rule where

import Prelude hiding ((.))
import Prologue

import           Control.Arrow
import           Control.Category
import           Control.Monad.Effect (Eff, Effect, Effectful (..))
import qualified Control.Monad.Effect as Eff
import           Control.Monad.Effect.State
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Machine
import           Data.Profunctor
import           Data.Text (Text, intercalate, unpack)

-- | The fundamental data type representing steps in a rewrite rule
-- from @from@ data to @to@ data. A Rule may never emit data, or it
-- might filter data, or it might be a 1:1 mapping from input to
-- output. A rule can access state and other side effects through the
-- @effs@ parameter.
--
-- A 'Rule' is a simple wrapper around the 'ProcessT' type from
-- @machines@. As such, it limits the input type of tokens to the
-- 'Is' carrier type, which might not be sufficiently flexible.
-- We may want to use 'MachineT' and make the machine's input
-- language explicit in the type of Rule:
-- @
--   data GRule input effs from to = GRule
--     { description :: [Text]
--     , machine :: MachineT (Eff effs) (k from) to
--     }
--
--   newtype Rule effs from to = Rule { unruly :: GRule Is effs from to }
-- @
--
-- This would allow us to use 'T' and 'Stack' in rules, which would be
-- pretty slick.
data Rule effs from to = Rule
  { description :: [Text]
  , machine     :: ProcessT (Eff effs) from to
  } deriving Functor

-- | The identity (pass-through) rule is 'id'. To compose 'Rule's
-- sequentially, use the `(>>>)` and `(<<<)` operators from
-- Control.Arrow.
instance Category (Rule effs) where
  id = Rule ["[anonymous] Rule.Category.id"] echo
  Rule d p . Rule d' p' = Rule (d' <> d) (p' ~> p)

-- | You can contravariantly map over the 'from' parameter and
-- covariantly map over the 'to' with 'lmap' and 'rmap' respectively
instance Profunctor (Rule effs) where
  lmap f (Rule d p) = Rule d (auto f ~> p)
  rmap = fmap

-- | You can use the Arrow combinators, or @-XArrows@ if you're really
-- feeling it.
instance Arrow (Rule effs) where
  arr = fromFunction "[anonymous] Rule.Arrow.arr"
  (Rule d a) *** (Rule d' b)
    = Rule (d <> d') (teeT zipping (auto fst ~> a) (auto snd ~> b))

-- | Rules contain 'description' values. They will generally have
-- at least one description, and will attempt, when composed, to yield
-- a list of descriptions that describes the composition to some degree.
instance Show (Rule effs from to) where
  show = unpack . intercalate " | " . description

-- | The empty 'Rule' is 'stopped' and does not accept any input.
instance Lower (Rule effs from to) where
  lowerBound = Rule ["[anonymous] lowerBound"] mempty

-- | Left-to-right composition.
instance Semigroup (Rule effs from to) where
  (Rule a c) <> (Rule b d) = Rule (a <> b) (c <> d)

instance Monoid (Rule effs from to)   where
  mempty = lowerBound
  mappend = (<>)

fromPlan :: Text -> PlanT (Is from) to (Eff effs) a -> Rule effs from to
fromPlan desc plan = Rule [desc] (repeatedly plan)

fromStateful :: Lower state => Text -> (state -> PlanT (Is from) to (Eff effs) state) -> Rule effs from to
fromStateful t = Rule [t] . unfoldPlan lowerBound

fromFunction :: Text -> (from -> to) -> Rule effs from to
fromFunction = fromAutomaton

fromEffect :: Text -> (from -> Eff effs to) -> Rule effs from to
fromEffect t = fromAutomatonM t . Kleisli

fromAutomaton :: Automaton k => Text -> k from to -> Rule effs from to
fromAutomaton t = Rule [t] . auto

fromAutomatonM :: AutomatonM k => Text -> k (Eff effs) from to -> Rule effs from to
fromAutomatonM t = Rule [t] . autoT

runRule :: Foldable f => f from -> Rule effs from to -> Eff effs [to]
runRule inp r = runT (source inp ~> machine r)
