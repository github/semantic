{-# LANGUAGE CPP #-}
module Semantic.Version
  ( buildSHA
  , buildVersion
  ) where


#if !BAZEL_BUILD
import Data.Version (showVersion)
import Paths_semantic (version)
#endif

-- The version string of this build of semantic.
buildVersion :: String
#if BAZEL_BUILD
buildVersion = "0.11.0.0"
#else
buildVersion = showVersion version
#endif

-- The SHA1 hash of this build of semantic.
-- If compiled as a development build, this will be @<development>@.
buildSHA :: String
buildSHA = "ba3f08b99f279bce1c2e2950f01a32306a5ad360"
