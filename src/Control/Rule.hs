{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, TypeOperators #-}
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

data Rule from to = Rule
  { description :: [Text]
  , machine     :: MachineT Identity (Is from) to
  } deriving (Functor)

instance Show (Rule from to) where
  show = unpack . intercalate " | " . description

instance Lower (Rule from to) where
  lowerBound = Rule [] mempty

instance Semigroup (Rule from to) where
  (Rule a c) <> (Rule b d) = Rule (a <> b) (c <> d)

data Previous a = After
  { previous :: Maybe a
  , current  :: a
  } deriving (Show, Eq, Functor)

fromMealy :: Text -> (Previous from -> to) -> Rule from to
fromMealy t f = Rule [t] . auto $ unfoldMealy go initial where
  initial = After Nothing (error shouldn'tHappen)
  shouldn'tHappen = "bug: attempted to access an After before it was ready"
  go acc from =
    let into = acc  { current = from }
        out  = into { previous = Just from }
    in (f into, out)


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
