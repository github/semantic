{-# LANGUAGE DeriveAnyClass #-}
module JSONTestCase where

import Data.Map.Strict as Map
import Prelude
import Prologue
import Data.Aeson

data JSONMetaRepo = JSONMetaRepo { repoPath :: !String
                                 , repoUrl :: !String
                                 , language :: !String
                                 , syntaxes :: ![JSONMetaSyntax]
                                 } deriving (Show, Generic, FromJSON)

data JSONMetaSyntax = JSONMetaSyntax { syntax :: !String
                                     , repoFilePath :: !String
                                     , testCaseFilePath :: !String
                                     , insert :: !String
                                     , replacement :: !String
                                     } deriving (Show, Generic, FromJSON)

data JSONTestCase = JSONTestCase { gitDir :: !String
                                 , testCaseDescription :: !String
                                 , filePaths :: ![String]
                                 , sha1 :: !String
                                 , sha2 :: !String
                                 , expectedResult :: !(Map Text (Map Text [Value]))
                                 } deriving (Show, Generic, FromJSON)

instance ToJSON JSONTestCase where
  toEncoding = genericToEncoding defaultOptions
