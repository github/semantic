module Data.ScopeGraph () where

import Source.Loc (Loc(..))
import Source.Source as Source

data ScopeGraph = ScopeGraph

class ToScopeGraph t where
    scopeGraph :: Source -> t Loc -> ScopeGraph

runScopeGraph :: Source -> ReaderC ScopeGraph Identity () -> ScopeGraph
runScopeGraph = undefined
