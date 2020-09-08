{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fprint-expanded-synonyms #-}

-- | This carrier interprets the Sketch effect, keeping track of
-- the current scope and in-progress graph internally.
module Control.Carrier.StackGraph
  ( StackGraphC,
    runStackGraph,
    module Control.Effect.StackGraph,
  )
where

import Analysis.Name (Name)
import qualified Analysis.Name as Name
import Control.Carrier.Error.Either
import Control.Carrier.Fresh.Strict
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Labelled
import qualified Control.Effect.StackGraph as StackGraph
import Control.Effect.StackGraph
import Data.Module (ModuleInfo)
import qualified Data.ScopeGraph as ScopeGraph
import Data.Semilattice.Lower
import Scope.Types
import qualified Stack.Graph as Stack

type StackGraphC addr m =
  StateC
    (Stack.Graph (Tagged Stack.Node))
    ( StateC
        (CurrentScope (Tagged Stack.Node))
        ( ReaderC
            (Tagged Stack.Node)
            ( ReaderC
                ModuleInfo
                ( ErrorC
                    ParseError
                    ( FreshC
                        (Labelled Tagged FreshC m)
                    )
                )
            )
        )
    )

runStackGraph ::
  (Functor m) =>
  ModuleInfo ->
  StackGraphC Name m a ->
  m (Either ParseError (Stack.Graph (Tagged Stack.Node), a))
runStackGraph minfo go =
  evalFresh 1
    . runLabelled
    . evalFresh 1
    . runError
    . runReader minfo
    . runReader rootNode
    . evalState (CurrentScope rootNode)
    . runState @(Stack.Graph (Tagged Stack.Node)) initialStackGraph
    $ go
  where
    rootname = Name.nameI 0
    rootNode = ((Stack.Scope rootname) Stack.:# 0)
    initialStackGraph = Stack.vertex rootNode
