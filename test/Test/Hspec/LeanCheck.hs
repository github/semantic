{-# LANGUAGE GADTs, TypeFamilies #-}
module Test.Hspec.LeanCheck
( prop
, forAll
) where

import Control.Exception
import Data.Bifunctor (first)
import Data.String (String)
import GHC.Show as Show (showsPrec)
import Prologue
import Test.Hspec
import Test.Hspec.Core.Spec
import Test.LeanCheck.Core

data Property where
  Property :: IOTestable prop => prop -> Property

prop :: (HasCallStack, IOTestable prop) => String -> prop -> Spec
prop s = it s . Property

data ForAll a where
  ForAll :: IOTestable prop => [[a]] -> (a -> prop) -> ForAll a

forAll :: IOTestable prop => [[a]] -> (a -> prop) -> ForAll a
forAll = ForAll

instance Example Property where
  type Arg Property = ()
  evaluateExample (Property prop) (Params _ bound) _ _ = do
    result <- iocounterExample bound prop
    case result of
      Just messages -> pure $ Fail Nothing (concat messages)
      Nothing -> pure Success

class IOTestable t where
  -- Like 'resultiers', but in 'IO'.
  ioresultiers :: t -> [[IO ([String], Bool)]]

instance IOTestable (IO ()) where
  ioresultiers action = [[ (action >> pure ([], True)) `catch` (\ e -> pure ([ displayException (e :: SomeException) ], False)) ]]

instance (IOTestable b, Show a, Listable a) => IOTestable (a -> b) where
  ioresultiers p = ioconcatMapT resultiersFor tiers
    where resultiersFor x = fmap (fmap (first (showsPrec 11 x "":))) <$> ioresultiers (p x)

instance IOTestable Bool where
  ioresultiers p = [[ pure ([], p) ]]

instance IOTestable (ForAll a) where
  ioresultiers (ForAll tiers property) = concatMapT (ioresultiers . property) tiers


ioconcatMapT :: (a -> [[IO b]]) -> [[a]] -> [[IO b]]
ioconcatMapT f = (>>= (>>= f))

iocounterExamples :: IOTestable a => Int -> a -> IO [[String]]
iocounterExamples n = fmap (fmap fst . filter (not . snd)) . sequenceA . take n . concat . ioresultiers

iocounterExample :: IOTestable a => Int -> a -> IO (Maybe [String])
iocounterExample n = fmap listToMaybe . iocounterExamples n
