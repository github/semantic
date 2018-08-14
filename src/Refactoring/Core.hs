{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}

module Refactoring.Core where

import Prologue

import Data.History
import Data.Term
import Data.Record

history :: (Annotated t (Record fields), HasField fields History) => t -> History
history = getField . annotation

-- ensureAccurateHistory :: ( term ~ Term s (Record fields)
--                          , Functor s
--                          , Foldable s
--                          , HasField fields History
--                          )
--                       => term -> term
-- ensureAccurateHistory t = foldSubterms historically t (history t)
--
-- historically :: ( term ~ Term s (Record fields)
--                 , Functor s
--                 , Foldable s
--                 , HasField fields History
--                 )
--              => SubtermAlgebra (Base term) term (History -> term)
-- historically f h
--   = embed (bimap (flip setField newHistory) extractTerm f) where
--     extractTerm (Subterm t c) = c . history $ t
--     childHistories = fmap (history . extractTerm) (toList f)
--     newHistory = revise h childHistories
