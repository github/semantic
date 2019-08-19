module Data.Language.Spec (testTree) where

import Data.Language
import Test.Tasty
import Test.Tasty.HUnit

testTree :: TestTree
testTree = testGroup "Data.Language"
  [ testCase "supportedExts returns expected list" $
      supportedExts @=? [".go",".rb",".builder",".eye",".fcgi",".gemspec",".god",".jbuilder",".mspec",".pluginspec",".podspec",".rabl",".rake",".rbuild",".rbw",".rbx",".ru",".ruby",".spec",".thor",".watchr",".py",".bzl",".cgi",".fcgi",".gyp",".gypi",".lmi",".py3",".pyde",".pyi",".pyp",".pyt",".pyw",".rpy",".spec",".tac",".wsgi",".xpy",".js","._js",".bones",".es",".es6",".frag",".gs",".jake",".jsb",".jscad",".jsfl",".jsm",".jss",".mjs",".njs",".pac",".sjs",".ssjs",".xsjs",".xsjslib",".ts",".php",".aw",".ctp",".fcgi",".inc",".php3",".php4",".php5",".phps",".phpt"]
  , testCase "codeNavLanguages returns expected list" $
      codeNavLanguages @=? [Go, Ruby, Python, JavaScript, TypeScript, PHP]
  ]
