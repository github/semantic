{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

import Control.Monad
import qualified Language.Python ()
import System.Path ((</>))
import qualified System.Path as Path
import qualified System.Path.Directory as Path
import qualified Test.Tasty as Tasty

{-

The Python code here is

hello = ()
goodbye = ()

The graph should be

  🏁
  |
  1️⃣----"hello"
  |
  |
  |
  |
  2️⃣----"goodbye"

-}

main :: IO ()
main = do
  -- make sure we're in the root directory so the paths resolve properly
  cwd <- Path.getCurrentDirectory
  when
    (Path.takeDirName cwd == Just (Path.relDir "semantic-python"))
    (Path.setCurrentDirectory (cwd </> Path.relDir ".."))

  Tasty.defaultMain $
    Tasty.testGroup
      "Tests"
      [ Tasty.testGroup
          "scope graph"
          []
      ]
