{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, ScopedTypeVariables, TypeOperators, LambdaCase #-}

-- | The fundamental data type representing steps in a rewrite rule
-- from @from@ data to @to@ data. A Rule may never emit data, or it
-- might filter data, or it might be a 1:1 mapping from input to
-- output. A rule can access state and other side effects through the
-- @effs@ parameter.
--
-- We use 'Rule's to build composable, constant-space pipelines for
-- streams of 'Token's; during the reprinting process, various
-- invariants may not be held over a token stream, and we can often
-- fix these invariants with simple state/stack machines. Indeed, in
-- this context a 'Rule' becomes a 'MachineT' with an effects list
-- inside it. In this sense, 'Rule's are similar to Bagge & Hasu's
-- concept of "rule-based token processors".
--
-- However, 'Rule's have a different purpose in the context of syntax
-- trees, rather than token streams: they are (well, will be)
-- convertible to a 'SubtermAlgebra' over a given 'Sum' type. In this
-- context, they are much more similar to the @Transform@ type from
-- the KURE rewriting system. Similarly, a 'Rule' from the matching
-- DSL should be convertible into a 'Rule'.

module Control.Rule
  ( Rule
  , description
  , machine
  -- * Constructing rules
  , fromPlan
  , fromStateful
  , fromEffect
  , fromAutomatonM
  , fromAutomaton
  , fromFunction
  , fromMatcher
  , toAlgebra
  , runRule
  ) where

import Prelude hiding ((.), id)
import Prologue

import           Control.Arrow
import           Control.Category
import           Control.Monad.Effect (Eff, Effect, Effectful (..))
import qualified Control.Monad.Effect as Eff
import           Control.Monad.Effect.State
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Machine
import           Data.Machine.Runner
import           Data.Profunctor
import           Data.Text (Text, intercalate, unpack)

import Control.Abstract.Matching


-- | A 'Rule' is a simple wrapper around the 'ProcessT' type from
-- @machines@. As such, it limits the input type of tokens to the
-- 'Is' carrier type, which might not be sufficiently flexible.
-- We may want to use 'MachineT' and make the machine's input
-- language explicit in the type of Rule:
-- @
--   data RuleC input effs from to = GRule
--     { description :: [Text]
--     , machine :: MachineT (Eff effs) (k from) to
--     }
--
--  type Rule = RuleC Is
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

instance ArrowChoice (Rule effs) where
  left l = fromPlan "left" go >>> rmap Left l where
    go = await >>= either yield (const (return ()))

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


-- | Build a 'Rule' from a description and a 'Plan'.  Plans allow you
-- to 'await' zero or more values from upstream and 'yield' zero or
-- more values downstream.  You can 'lift' effectful actions into
-- 'PlanT'.
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

fromMatcher :: Text -> Matcher from to -> Rule effs from (Either from (from, to))
fromMatcher t m = Rule [t] (auto go) where go x = maybe (Left x) (\y -> Right (x, y)) (runOnce x m)

fromAutomatonM :: AutomatonM k => Text -> k (Eff effs) from to -> Rule effs from to
fromAutomatonM t = Rule [t] . autoT

toAlgebra :: (Traversable (Base t), Corecursive t)
          => Rule effs t t
          -> FAlgebra (Base t) (Eff effs t)
toAlgebra (Rule _ m) t = do
  inner <- sequenceA t
  res <- runT1 (source (Just (embed inner)) ~> m)
  pure (fromMaybe (embed inner) res)

runRule :: Foldable f => f from -> Rule effs from to -> Eff effs [to]
runRule inp r = runT (source inp ~> machine r)
