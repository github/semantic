-- | Semantic functionality for JSON programs.
module Language.JSON
( Term(..)
) where

import qualified TreeSitter.JSON.AST as JSON
import qualified TreeSitter.Unmarshal as TS

newtype Term a = Term { getTerm :: JSON.Document a }

instance TS.Unmarshal Term where
  unmarshalNode node = Term <$> TS.unmarshalNode node
