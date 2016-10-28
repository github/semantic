{-# LANGUAGE DeriveAnyClass, OverloadedStrings #-}
module JSONTestCase where

import Data.Aeson
import Data.Aeson.Types
import Data.Map.Strict as Map
import Data.HashMap.Strict as HM
import Prelude
import Prologue

data JSONMetaRepo = JSONMetaRepo { repoPath :: !String
                                 , repoUrl  :: !String
                                 , language :: !String
                                 , syntaxes :: ![JSONMetaSyntax]
                                 } deriving (Show, Generic, FromJSON)

data JSONMetaSyntax = JSONMetaSyntax { syntax           :: !String
                                     , repoFilePath     :: !String
                                     , testCaseFilePath :: !String
                                     , insert           :: !String
                                     , replacement      :: !String
                                     } deriving (Show, Generic, FromJSON)

data JSONTestCase = JSONTestCase { gitDir              :: !String
                                 , testCaseDescription :: !String
                                 , filePaths           :: ![String]
                                 , sha1                :: !String
                                 , sha2                :: !String
                                 , expectedResult      :: !ExpectedResult
                                 } deriving (Show, Generic, FromJSON)

data ExpectedResult = SummaryResult (Map Text (Map Text [Value]))
                    | JSONResult (Map Text Value)
                    | EmptyResult
                    deriving (Show, Generic, FromJSON)

-- | These replace the defaultOptions normally used by genericToEncoding.
-- | All options are default except for `sumEncoding`, which uses the `UntaggedValue`
-- | option to prevent the sum type `ExpectedResult` from encoding with a `tag` and `contents`
-- | fields when a JSONTestCase is encoded.
jsonTestCaseOptions :: Options
jsonTestCaseOptions = Options { fieldLabelModifier = id
                              , constructorTagModifier = id
                              , allNullaryToStringTag = False
                              , omitNothingFields = True
                              , sumEncoding = UntaggedValue
                              , unwrapUnaryRecords = False
                              }

instance ToJSON JSONTestCase where
  toJSON = genericToJSON jsonTestCaseOptions
  toEncoding = genericToEncoding jsonTestCaseOptions

instance ToJSON ExpectedResult where
  toJSON = genericToJSON jsonTestCaseOptions
  toEncoding = genericToEncoding jsonTestCaseOptions

-- | We have to parse the specific formats of the ExpectedResults based on their keys.
-- | This is how we determine which ExpectedResult constructor to use.
instance FromJSON ExpectedResult where
  parseJSON = Data.Aeson.withObject "ExpectedResult" $ \o ->
    SummaryResult <$> summaryResultValues o <|>
    JSONResult <$> jsonResultValues o <|>
    EmptyResult <$> o .: ""
    where
      jsonResultValues :: Object -> Parser (Map Text Value)
      jsonResultValues o = Map.fromList <$> (fromKey "oids" <> fromKey "rows" <> fromKey "paths")
        where fromKey k = (\a -> [(k, a)]) <$> o .: k
      summaryResultValues :: Object -> Parser (Map Text (Map Text [Value]))
      summaryResultValues o = Map.fromList <$> (fromKey "changes" <> fromKey "errors")
        where fromKey k = (\a -> [(k :: Text, Map.fromList . HM.toList $ a )] ) <$> o .: k
