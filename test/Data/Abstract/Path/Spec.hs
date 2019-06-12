{-# LANGUAGE TemplateHaskell #-}

module Data.Abstract.Path.Spec(spec) where

import Data.Abstract.Path
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

spec :: TestTree
spec = $(testGroupGenerator)

case_joins_empty_paths = joinPaths "" "" @?= "."
case_joins_relative_paths = joinPaths "a/b" "./c" @?= "a/b/c"
case_joins_absolute_paths = joinPaths "/a/b" "c" @?= "/a/b/c"
case_walks_up_directories = joinPaths "a/b" "../c" @?= "a/c"
case_walks_up_multiple_directories = joinPaths "a/b" "../../c" @?= "c"
case_stops_walking_at_top_directory = joinPaths "a/b" "../../../c" @?= "c"
