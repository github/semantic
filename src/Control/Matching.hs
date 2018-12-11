{-# LANGUAGE GADTs, TypeOperators #-}

module Control.Matching
  ( module X
  , Matcher
  ) where

import Control.Rewriting as X

type Matcher = Rewrite
