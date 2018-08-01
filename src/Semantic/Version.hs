{-# OPTIONS_GHC -fforce-recomp #-} -- So that gitHash is correct.
{-# LANGUAGE TemplateHaskell #-}
module Semantic.Version
  ( buildSHA
  , buildVersion
  ) where

import Data.Version (showVersion)
import Development.GitRev
import Paths_semantic (version)

-- The SHA1 hash of this build of semantic.
buildSHA :: String
buildSHA = $(gitHash)

-- The version string of this build of semantic.
buildVersion :: String
buildVersion = showVersion version
