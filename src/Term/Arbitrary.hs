module Term.Arbitrary where

import Prologue
import Term

newtype ArbitraryTerm leaf annotation = ArbitraryTerm { unArbitraryTerm :: TermF leaf annotation (ArbitraryTerm leaf annotation) }
  deriving (Show, Eq, Generic)
