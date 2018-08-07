{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TypeOperators, GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Control.Rule where

import Prologue

import           Control.Monad.Effect (Eff, Effect, Effectful (..))
import qualified Control.Monad.Effect as Eff
import           Control.Monad.Effect.State
import           Data.Coerce
import           Data.Functor.Identity
import           Data.Machine
import           Data.Profunctor
import           Data.Text (Text, intercalate, unpack)

-- | The fundamental data type representing a rewrite rule
-- from 'from' data to 'to' data. A Rule may never emit data,
-- or it might filter data, or it might be a 1:1 mapping from
-- input to output.
data Rule m from to where
  Rule :: Monad m => [Text] -> ProcessT m from to -> Rule m from to

description :: Rule m from to -> [Text]
description (Rule d _) = d

machine :: Rule m from to -> ProcessT m from to
machine (Rule _ m) = m

instance Show (Rule m from to) where
  show = unpack . intercalate " | " . description

instance Monad m => Lower (Rule m from to) where
  lowerBound = Rule [] mempty

instance Semigroup (Rule m from to) where
  (Rule a c) <> (Rule b d) = Rule (a <> b) (c <> d)

fromPlan :: Monad m => Text -> Plan (Is from) to () -> Rule m from to
fromPlan desc plan = Rule [desc] (repeatedly plan)

-- fromFunction :: Text -> (from -> to) -> Rule from to
-- fromFunction t = Rule [t] . auto

-- justs :: Rule (Maybe it) it
-- justs = Rule "[builtin] justs"

-- fromMealy :: Text -> Plan from to () -> Rule from to
-- fromMealy t f = Rule [t] . auto $ unfoldMealy go initial where
--   initial = After Nothing (error shouldn'tHappen)
--   shouldn'tHappen = "bug: attempted to access an After before it was ready"
--   go acc from =
--     let into = acc  { current = from }
--         out  = into { previous = Just from }
--     in (f into, out)


-- remembering :: Rule effs from (After from)
-- remembering =



-- instance Monoid (Rule effs from to) where
--   mappend = (<>)
--   mempty  = lowerBound

-- -- instance Profunctor (Rule effs) where
-- --   dimap f g (Rule t m) = Rule t _

-- -- | This is a natural transformation between 'effs' and 'Identity'.
-- -- type Purifier effs = forall a . Eff effs a -> a

-- -- runRule :: Foldable f => (Eff effs a -> a) -> Rule effs from to -> f from -> [to]
-- -- runRule p (Rule _ m) s = Eff.run .  . runT $ source s ~> m
-- --p . snd . runState lowerBound . runT

-- inside :: (forall a . Eff old a -> Eff new a) -> Rule old from to -> Rule new from to
-- inside f (Rule d m) = Rule d (fitM f m)

-- toProcess :: Effect (Union effs) => Rule effs from to -> ProcessT (Eff effs) from to
-- toProcess = machine

-- runRuleM :: (Effect (Union effs), Effectful m, Foldable f) => Rule effs from to -> f from -> m effs [to]
-- runRuleM (Rule _ mach) src
--   = raiseEff
--   . runT $ source src ~> mach
