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
    (ScopeGraph Name)
    ( StateC
        (Stack.Graph Stack.Node)
        ( StateC
            (CurrentScope Name)
            ( ReaderC
                Stack.Node
                ( ReaderC
                    ModuleInfo
                    ( ErrorC
                        ParseError
                        ( FreshC
                            (Labelled StackGraph.Tagged FreshC m)
                        )
                    )
                )
            )
        )
    )

runStackGraph ::
  (Functor m) =>
  ModuleInfo ->
  StackGraphC Name m a ->
  m (Either ParseError (Stack.Graph Stack.Node, (ScopeGraph Name, a)))
runStackGraph minfo go =
  evalFresh 0
    . runLabelled
    . evalFresh 1
    . runError
    . runReader minfo
    . runReader (Stack.Scope rootname)
    . evalState (CurrentScope rootname)
    . runState @(Stack.Graph Stack.Node) initialStackGraph
    . runState @(ScopeGraph Name) initialGraph
    $ go
  where
    rootname = Name.nameI 0
    initialGraph = ScopeGraph.insertScope rootname lowerBound lowerBound
    initialStackGraph = Stack.scope rootname
