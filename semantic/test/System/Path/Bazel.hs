{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module System.Path.Bazel
  ( bazelFile,
    HasBazel,
  )
where

import qualified Bazel.Runfiles as Bazel
import qualified System.Path as Path

type HasBazel = ?runfiles :: Bazel.Runfiles

bazelFile :: HasBazel => String -> Path.AbsFile
bazelFile x = Path.absFile (Bazel.rlocation ?runfiles ("semantic/semantic/" <> x))
