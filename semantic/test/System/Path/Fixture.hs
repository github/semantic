{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module System.Path.Fixture
  ( absFile,
    absRelFile,
    bazelDir,
    HasFixture,
    absRelDir,
    delay,
  )
where

import qualified Bazel.Runfiles as Bazel
import Data.Proxy
import GHC.Stack
import GHC.TypeLits
import qualified System.Path as Path
import Control.Concurrent
import System.IO

type HasFixture =
  ( ?runfiles :: Bazel.Runfiles,
    ?project :: Path.RelDir,
    HasCallStack
  )

delay :: String -> IO ()
delay s = do
  putStrLn s
  hFlush stdout
  threadDelay 100000000

absFile :: (HasFixture) => String -> Path.AbsFile
absFile x = Path.absFile (Bazel.rlocation ?runfiles ("semantic/" <> Path.toString ?project <> x))

absRelFile :: (HasFixture) => String -> Path.AbsRelFile
absRelFile = Path.toAbsRel . absFile

bazelDir :: HasFixture => String -> Path.AbsDir
bazelDir x = Path.absDir (Bazel.rlocation ?runfiles ("semantic/" <> Path.toString ?project <> x))

absRelDir :: HasFixture => String -> Path.AbsRelDir
absRelDir = Path.toAbsRel . bazelDir
