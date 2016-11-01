{-# OPTIONS_GHC -fno-warn-orphans #-}
module Term.Instances where

import Prologue
import Data.Record
import Term
import Data.Aeson

instance (ToJSON leaf, ToJSON (Record fields)) => ToJSON (SyntaxTerm leaf fields) where
  toJSON syntaxTerm = case runCofree syntaxTerm of
    (record :< syntax) -> object [ ("record", toJSON record), ("syntax", toJSON syntax) ]
