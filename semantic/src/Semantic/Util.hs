{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-exported-signatures -Wno-partial-type-signatures -O0 #-}
module Semantic.Util
  ( parseFile
  , parseFileQuiet
  ) where

import Prelude hiding (readFile)

import           Analysis.File
import           Control.Carrier.Parse.Simple
import           Control.Effect.Reader
import           Control.Exception hiding (evaluate)
import           Control.Monad
import           Parsing.Parser
import           Semantic.Config
import           Semantic.Task
import qualified Source.Language as Language
import           Source.Span (Pos (..), point)
import           System.Exit (die)
import qualified System.Path as Path

parseFile, parseFileQuiet :: Parser term -> FilePath -> IO term
parseFile      parser = runTask'     . (parse parser <=< readBlob . fileForPath)
parseFileQuiet parser = runTaskQuiet . (parse parser <=< readBlob . fileForPath)

fileForPath :: FilePath -> File Language.Language
fileForPath (Path.absRel -> p) = File p (point (Pos 1 1)) (Language.forPath p)

runTask', runTaskQuiet :: ParseC TaskC a -> IO a
runTask'     task = runTaskWithOptions debugOptions   (asks configTreeSitterParseTimeout >>= \ timeout -> runParse timeout task) >>= either (die . displayException) pure
runTaskQuiet task = runTaskWithOptions defaultOptions (asks configTreeSitterParseTimeout >>= \ timeout -> runParse timeout task) >>= either (die . displayException) pure
