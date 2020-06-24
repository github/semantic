{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-exported-signatures -Wno-partial-type-signatures -O0 #-}
module Semantic.Util
  ( mergeErrors
  , reassociate
  , parseFile
  , parseFileQuiet
  ) where

import Prelude hiding (readFile)

import           Analysis.File
import           Control.Abstract
import           Control.Carrier.Parse.Simple
import           Control.Carrier.Resumable.Either (SomeError (..))
import           Control.Exception hiding (evaluate)
import           Control.Monad
import qualified Data.Language as Language
import           Data.Sum
import           Parsing.Parser
import           Semantic.Config
import           Semantic.Task
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

mergeErrors :: Either (SomeError (Sum errs)) (Either (SomeError err) result) -> Either (SomeError (Sum (err ': errs))) result
mergeErrors = either (\ (SomeError sum) -> Left (SomeError (weaken sum))) (either (\ (SomeError err) -> Left (SomeError (inject err))) Right)

reassociate :: Either (SomeError err1) (Either (SomeError err2) (Either (SomeError err3) (Either (SomeError err4) (Either (SomeError err5) (Either (SomeError err6) (Either (SomeError err7) (Either (SomeError err8) result))))))) -> Either (SomeError (Sum '[err8, err7, err6, err5, err4, err3, err2, err1])) result
reassociate = mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . mergeErrors . Right
