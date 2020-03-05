module Data.Language.Spec (testTree) where

import           Data.Language as Language
import qualified System.Path as Path
import           Test.Tasty
import           Test.Tasty.HUnit

testTree :: TestTree
testTree = testGroup "Data.Language"
  [ testCase "supportedExts returns expected list" $
      supportedExts @=? [".go",".java",".rb",".builder",".eye",".fcgi",".gemspec",".god",".jbuilder",".mspec",".pluginspec",".podspec",".rabl",".rake",".rbi",".rbuild",".rbw",".rbx",".ru",".ruby",".spec",".thor",".watchr",".py",".cgi",".fcgi",".gyp",".gypi",".lmi",".py3",".pyde",".pyi",".pyp",".pyt",".pyw",".rpy",".smk",".spec",".tac",".wsgi",".xpy",".js","._js",".bones",".cjs",".es",".es6",".frag",".gs",".jake",".jsb",".jscad",".jsfl",".jsm",".jss",".mjs",".njs",".pac",".sjs",".ssjs",".xsjs",".xsjslib",".ts",".php",".aw",".ctp",".fcgi",".inc",".php3",".php4",".php5",".phps",".phpt"]

  , testCase "codeNavLanguages returns expected list" $
      codeNavLanguages @=? [Go, Java, Ruby, Python, JavaScript, TypeScript, PHP]

  , testCase "languageForFilePath works for languages with ambiguous lingo extensions" $ do
      Language.forPath (Path.relFile "foo.php") @=? PHP
      Language.forPath (Path.relFile "foo.md" ) @=? Markdown
      Language.forPath (Path.relFile "foo.tsx") @=? TSX
  ]
