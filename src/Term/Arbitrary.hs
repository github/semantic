module Term.Arbitrary where

import Data.Functor.Foldable (unfold)
import Prologue
import Term

newtype ArbitraryTerm leaf annotation = ArbitraryTerm { unArbitraryTerm :: TermF leaf annotation (ArbitraryTerm leaf annotation) }
  deriving (Show, Eq, Generic)

toTerm :: ArbitraryTerm leaf annotation -> Term leaf annotation
toTerm = unfold unArbitraryTerm
