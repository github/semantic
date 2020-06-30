{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module System.Path.Fixture
  ( absFile,
    absRelFile,
    bazelDir,
    HasFixture,
    absRelDir,
  )
where

import qualified Bazel.Runfiles as Bazel
import Data.Proxy
import GHC.Stack
import GHC.TypeLits
import qualified System.Path as Path

type HasFixture =
  ( ?runfiles :: Bazel.Runfiles,
    ?project :: Path.RelDir,
    HasCallStack
  )

absFile :: (HasFixture) => String -> Path.AbsFile
absFile x = Path.absFile (Bazel.rlocation ?runfiles ("semantic/" <> Path.toString ?project <> x))

absRelFile :: (HasFixture) => String -> Path.AbsRelFile
absRelFile = Path.toAbsRel . absFile

bazelDir :: HasFixture => String -> Path.AbsDir
bazelDir x = Path.absDir (Bazel.rlocation ?runfiles ("semantic/" <> Path.toString ?project <> x))

absRelDir :: HasFixture => String -> Path.AbsRelDir
absRelDir = Path.toAbsRel . bazelDir
