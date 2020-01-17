{-# LANGUAGE DeriveGeneric, DeriveTraversable, GeneralizedNewtypeDeriving, LambdaCase, OverloadedLists #-}
module Core.Name
( module Analysis.Functor.Named
, reservedNames
, isSimpleCharacter
, needsQuotation
) where

import           Analysis.Functor.Named
import qualified Data.Char as Char
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text as Text (any, unpack)

reservedNames :: HashSet String
reservedNames = [ "#true", "#false", "if", "then", "else"
                , "#unit", "load", "rec", "#record"]

-- | Returns true if any character would require quotation or if the
-- name conflicts with a Core primitive.
needsQuotation :: Name -> Bool
needsQuotation (Name u) = HashSet.member (unpack u) reservedNames || Text.any (not . isSimpleCharacter) u
needsQuotation _        = False

-- | A ‘simple’ character is, loosely defined, a character that is compatible
-- with identifiers in most ASCII-oriented programming languages. This is defined
-- as the alphanumeric set plus @$@ and @_@.
isSimpleCharacter :: Char -> Bool
isSimpleCharacter = \case
  '$'  -> True -- common in JS
  '_'  -> True
  '?'  -> True -- common in Ruby
  c    -> Char.isAlphaNum c
