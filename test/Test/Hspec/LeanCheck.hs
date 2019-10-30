{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}
module Test.Hspec.LeanCheck
( prop
, forAll
) where

import Control.Exception
import Data.Bifunctor (first)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import GHC.Show as Show (showsPrec)
import Test.Hspec
import Test.Hspec.Core.Spec as Hspec
import qualified Test.HUnit.Lang as HUnit
import Test.LeanCheck.Core

data Property where
  Property :: IOTestable prop => prop -> Property

-- | Perform an enumerative test of a property using LeanCheck.
--
--   'prop' will typically be a function of one or more 'Listable' arguments, returning either 'Bool' or 'IO ()' (in the latter case, typically via 'shouldBe' and friends). For example:
--
-- > describe "+" $ do
-- >   prop "associativity" $
-- >     \ a b c -> a + (b + c) `shouldBe` (a + b :: Int) + c
prop :: (HasCallStack, IOTestable prop) => String -> prop -> Spec
prop s = it s . Property

data ForAll a where
  ForAll :: IOTestable prop => [[a]] -> (a -> prop) -> ForAll a

-- | Test a property given by an explicit list of tiers rather than a 'Listable' instance. This can be used to e.g. filter input values for which the property does not hold.
--
-- > describe "mean" $ do
-- >   prop "≥ the minimum" . forAll (not . null `filterT` tiers) $
-- >     \ list -> (mean list :: Int) `shouldSatisfy` (>= min list)
forAll :: IOTestable prop => [[a]] -> (a -> prop) -> ForAll a
forAll = ForAll

instance Example Property where
  type Arg Property = ()
  evaluateExample (Property prop) (Params _ bound) _ _ = do
    result <- try (iocounterExample bound prop)
    case result of
      Left e
        | Just (LeanCheckException messages e') <- fromException e -> throw (addMessages messages e')
        | otherwise -> throw e
      Right (Just messages) -> pure $ Result "" (Failure Nothing (Reason (concat messages)))
      Right Nothing -> pure $ Result "" Success
    where addMessages messages (HUnit.HUnitFailure loc r) = HUnit.HUnitFailure loc $ case r of
            HUnit.Reason s -> HUnit.Reason (intercalate "\n" messages ++ "\n" ++ s)
            HUnit.ExpectedButGot Nothing expected actual -> HUnit.ExpectedButGot (Just (concat messages)) expected actual
            HUnit.ExpectedButGot (Just preface) expected actual -> HUnit.ExpectedButGot (Just (intercalate "\n" messages ++ preface)) expected actual


class IOTestable t where
  -- 'resultiers', lifted into 'IO'.
  ioResultTiers :: t -> [[IO ([String], Bool)]]

instance IOTestable (IO ()) where
  ioResultTiers action = [[ (action >> pure ([], True)) `catch` (throw . LeanCheckException []) ]]

instance (IOTestable b, Show a, Listable a) => IOTestable (a -> b) where
  ioResultTiers p = concatMapT (resultiersFor p) tiers

instance IOTestable Bool where
  ioResultTiers p = [[ pure ([], p) ]]

instance Show a => IOTestable (ForAll a) where
  ioResultTiers (ForAll tiers property) = concatMapT (resultiersFor property) tiers

resultiersFor :: (IOTestable b, Show a) => (a -> b) -> a -> [[IO ([String], Bool)]]
resultiersFor p x = fmap (eval x) <$> ioResultTiers (p x)

eval :: Show a => a -> IO ([String], Bool) -> IO ([String], Bool)
eval x action = first (prepend x) <$> action
  `catch` \ (LeanCheckException messages failure) -> throw (LeanCheckException (prepend x messages) failure)
  where prepend x = (showsPrec 11 x "":)


-- | 'counterExamples', lifted into 'IO'.
iocounterExamples :: IOTestable a => Int -> a -> IO [[String]]
iocounterExamples n = fmap (fmap fst . filter (not . snd)) . sequenceA . take n . concat . ioResultTiers

-- | 'counterExample', lifted into 'IO'.
iocounterExample :: IOTestable a => Int -> a -> IO (Maybe [String])
iocounterExample n = fmap listToMaybe . iocounterExamples n


data LeanCheckException = LeanCheckException [String] HUnit.HUnitFailure
  deriving (Show)

instance Exception LeanCheckException
