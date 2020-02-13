{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Name
( module Analysis.Functor.Named
, reservedNames
, isSimpleCharacter
, needsQuotation
) where

import           Analysis.Functor.Named
import qualified Data.Char as Char
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.String
import           Data.Text as Text (any)

reservedNames :: (Eq s, IsString s, Hashable s) => HashSet s
reservedNames = [ "#true", "#false", "if", "then", "else"
                , "#unit", "load", "rec", "#record"]

-- | Returns true if any character would require quotation or if the
-- name conflicts with a Core primitive.
needsQuotation :: Name -> Bool
needsQuotation n
  | isGenerated n = False
  | otherwise     = HashSet.member n reservedNames || Text.any (not . isSimpleCharacter) (formatName n)

-- | A ‘simple’ character is, loosely defined, a character that is compatible
-- with identifiers in most ASCII-oriented programming languages. This is defined
-- as the alphanumeric set plus @$@ and @_@.
isSimpleCharacter :: Char -> Bool
isSimpleCharacter = \case
  '$'  -> True -- common in JS
  '_'  -> True
  '?'  -> True -- common in Ruby
  c    -> Char.isAlphaNum c
