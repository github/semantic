{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}

module Matching.Core
( matchHash
, matchArray
, matchFloat
) where

import Prologue

import           Control.Matching
import qualified Data.Syntax.Literal as Literal
import           Data.Term

matchHash :: (Literal.Hash :< fs, term ~ Term (Sum fs) ann) => Matcher term (Literal.Hash term)
matchHash = matchM projectTerm target

matchArray :: (Literal.Array :< fs, term ~ Term (Sum fs) ann) => Matcher term (Literal.Array term)
matchArray = matchM projectTerm target

matchFloat :: (Literal.Float :< fs, term ~ Term (Sum fs) ann) => Matcher term (Literal.Float term)
matchFloat = matchM projectTerm target
