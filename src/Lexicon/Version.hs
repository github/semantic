{-# LANGUAGE CPP #-}
#ifdef COMPUTE_GIT_SHA
{-# OPTIONS_GHC -fforce-recomp #-} -- So that gitHash is correct.
{-# LANGUAGE TemplateHaskell #-}
#endif
module Lexicon.Version
  ( buildSHA
  , buildVersion
  ) where

import Data.Version (showVersion)
#ifdef COMPUTE_GIT_SHA
import Development.GitRev
#endif
import Paths_lexicon (version)

-- The SHA1 hash of this build of lexicon.
-- If compiled as a development build, this will be @<development>@.
buildSHA :: String
#ifdef COMPUTE_GIT_SHA
buildSHA = $(gitHash)
#else
buildSHA = "<development>"
#endif

-- The version string of this build of lexicon.
buildVersion :: String
buildVersion = showVersion version
