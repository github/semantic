module Split where

import Diff
import Patch
import Syntax
import Term
import Unified
import Control.Comonad.Cofree
import Rainbow

type ClassName = String
type Element a = Cofree (Syntax a) (Maybe ClassName, String)

split :: Diff a Info -> String -> String -> IO ByteString
split _ _ _ = return mempty

splitDiff :: Diff a Info -> String -> String -> [(String, String)]
splitDiff _ _ _ = []

splitPatch :: Patch (Term a Info) -> String -> String -> [(String, String)]
splitPatch _ _ _ = []

splitTerm :: String -> Term a Info -> Element a
splitTerm source term = toElement term where
  toElement ((Info range categories) :< syntax) = (foldr (const . Just) Nothing categories, substring range source) :< fmap toElement syntax
