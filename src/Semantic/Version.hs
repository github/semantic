{-# OPTIONS_GHC -fforce-recomp #-} -- So that gitHash is correct.
{-# LANGUAGE TemplateHaskell #-}
module Semantic.Version where

import           Data.Version (showVersion)
import           Development.GitRev
import qualified Paths_semantic as Library (version)

buildSHA :: String
buildSHA = $(gitHash)

buildVersion :: String
buildVersion = showVersion Library.version
