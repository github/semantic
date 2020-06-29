module Semantic.Version
  ( buildSHA
  , buildVersion
  ) where

import Data.Version (showVersion)
import Paths_semantic (version)

-- The version string of this build of semantic.
buildVersion :: String
buildVersion = showVersion version

-- The SHA1 hash of this build of semantic.
-- If compiled as a development build, this will be @<development>@.
buildSHA :: String
buildSHA = "<development>"
