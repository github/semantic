module Split where

import Diff
import Patch
import Syntax
import Term
import Unified
import qualified Data.Set as Set
import Rainbow

data Node = Node String (Set.Set Category)

split :: Diff a Info -> String -> String -> IO ByteString
split _ _ _ = return mempty

splitDiff :: Diff a Info -> String -> String -> [(String, String)]
splitDiff _ _ _ = []

splitPatch :: Patch (Term a Info) -> String -> String -> [(String, String)]
splitPatch _ _ _ = []

splitTerm :: String -> Term a Info -> [Term a Node]
splitTerm source term = splitUp $ fmap toNode term where
  toNode (Info range categories) = Node (substring range source) categories
  splitUp :: Term a Node -> [Term a Node]
  splitUp term = [term]
