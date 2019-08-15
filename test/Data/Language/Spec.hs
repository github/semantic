module Data.Language.Spec (spec) where

import Data.Language
import SpecHelpers

spec :: Spec
spec = describe "Data.Language" $ do
  it "supportedExts returns expected list" $
    supportedExts `shouldBe` [".go",".rb",".builder",".eye",".fcgi",".gemspec",".god",".jbuilder",".mspec",".pluginspec",".podspec",".rabl",".rake",".rbuild",".rbw",".rbx",".ru",".ruby",".spec",".thor",".watchr",".py",".bzl",".cgi",".fcgi",".gyp",".gypi",".lmi",".py3",".pyde",".pyi",".pyp",".pyt",".pyw",".rpy",".spec",".tac",".wsgi",".xpy",".js","._js",".bones",".es",".es6",".frag",".gs",".jake",".jsb",".jscad",".jsfl",".jsm",".jss",".mjs",".njs",".pac",".sjs",".ssjs",".xsjs",".xsjslib",".ts",".php",".aw",".ctp",".fcgi",".inc",".php3",".php4",".php5",".phps",".phpt"]

  it "codeNavLanguages returns expected list" $
    codeNavLanguages `shouldBe` [Go, Ruby, Python, JavaScript, TypeScript, PHP]
