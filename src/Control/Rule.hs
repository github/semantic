{-# LANGUAGE GADTs, GeneralizedNewtypeDeriving, KindSignatures, RankNTypes, TypeOperators #-}
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
data Rule effs from to = Rule
  { description :: [Text]
  , machine     :: ProcessT (Eff effs) from to
  } deriving Functor

instance Category (Rule effs) where
  id = Rule ["[builtin] echo"] echo
  Rule d p . Rule d' p' = Rule (d' <> d) (p' ~> p)

instance Profunctor (Rule effs) where
  lmap f (Rule d p) = Rule d (auto f ~> p)
  rmap = fmap

instance Show (Rule effs from to) where
  show = unpack . intercalate " | " . description

instance Lower (Rule effs from to) where
  lowerBound = Rule [] mempty

instance Semigroup (Rule effs from to) where
  (Rule a c) <> (Rule b d) = Rule (a <> b) (c <> d)

instance Monoid (Rule effs from to)   where
  mempty = lowerBound
  mappend = (<>)

fromPlan :: Text -> PlanT (Is from) to (Eff effs) () -> Rule effs from to
fromPlan desc plan = Rule [desc] (repeatedly plan)

fromPlanState :: Lower state => Text -> (state -> PlanT (Is from) to (Eff effs) state) -> Rule effs from to
fromPlanState t = Rule [t] . unfoldPlan lowerBound

fromFunction :: Text -> (from -> to) -> Rule effs from to
fromFunction t f = fromEffect t (pure . f)

fromEffect :: Text -> (from -> Eff effs to) -> Rule effs from to
fromEffect t = Rule [t] . autoT . Kleisli

runRule :: Foldable f => f from -> Rule effs from to -> Eff effs [to]
runRule inp r = runT (source inp ~> machine r)

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
