{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}

module System.Path.Fixture
  ( absRelFile,
    HasFixture,
    absRelDir,
    delay,
    create,
  )
where

import Control.Concurrent
import GHC.Stack
import System.IO
import System.Path ((</>))
import qualified System.Path as Path

#if BAZEL_BUILD

import qualified Bazel.Runfiles as Bazel

type HasFixture =
  ( ?runfiles :: Bazel.Runfiles,
    ?project :: Path.RelDir,
    HasCallStack
  )

create :: IO Bazel.Runfiles
create = Bazel.create

root :: HasFixture => Path.AbsRelDir
root = Path.absRel (Bazel.rlocation ?runfiles ".")

absRelFile :: (HasFixture) => String -> Path.AbsRelFile
absRelFile x = Path.toAbsRel (root </> Path.relDir "semantic" </> ?project </> Path.relFile x)

absRelDir :: HasFixture => String -> Path.AbsRelDir
absRelDir x = Path.toAbsRel (root </> Path.relDir "semantic" </> ?project </> Path.relDir x)


#else
-- building under Cabal

type HasFixture = HasCallStack

create :: IO ()
create = pure ()

root :: HasFixture => Path.AbsRelDir
root = Path.toAbsRel Path.currentDir

absRelFile :: String -> Path.AbsRelFile
absRelFile = Path.absRel

absRelDir :: String -> Path.AbsRelDir
absRelDir = Path.absRel

#endif

delay :: String -> IO ()
delay s = do
  putStrLn s
  hFlush stdout
  threadDelay 100000000
