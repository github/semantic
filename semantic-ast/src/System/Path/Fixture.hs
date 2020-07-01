{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module System.Path.Fixture
  ( absRelFile,
    relDir,
    HasFixture,
    absRelDir,
    delay,
  )
where

import qualified Bazel.Runfiles as Bazel
import Control.Concurrent
import GHC.Stack
import System.IO
import System.Path ((</>))
import qualified System.Path as Path

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


absRelFile :: (HasFixture) => String -> Path.AbsRelFile
absRelFile x = Path.toAbsRel (root </> Path.relDir "semantic" </> ?project </> Path.relFile x)
  where
    root = Path.absDir (Bazel.rlocation ?runfiles ".")


relDir :: HasFixture => String -> Path.AbsDir
relDir x = root </> Path.relDir "semantic" </> ?project </> Path.relDir x
  where
    root = Path.absDir (Bazel.rlocation ?runfiles ".")

absRelDir :: HasFixture => String -> Path.AbsRelDir
absRelDir = Path.toAbsRel . relDir
