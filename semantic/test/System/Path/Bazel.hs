{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module System.Path.Bazel
  ( bazelFile,
    bazelFile',
    bazelDir,
    HasBazel,
  )
where

import qualified Bazel.Runfiles as Bazel
import qualified System.Path as Path
import GHC.Stack

type HasBazel = (?runfiles :: Bazel.Runfiles, HasCallStack)

bazelFile :: (HasBazel) => String -> Path.AbsFile
bazelFile x = Path.absFile (Bazel.rlocation ?runfiles ("semantic/semantic/" <> x))

bazelFile' :: (HasBazel) => String -> Path.AbsRelFile
bazelFile' = Path.toAbsRel . bazelFile

bazelDir :: HasBazel => String -> Path.AbsDir
bazelDir x = Path.absDir (Bazel.rlocation ?runfiles ("semantic/semantic/" <> x))
