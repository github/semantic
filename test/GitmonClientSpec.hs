module GitmonClientSpec where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy (fromChunks)
import Data.ByteString.Char8 (split, ByteString)
import Data.Maybe (fromJust)
import Data.Text hiding (split, take)
import Git.Libgit2
import Git.Repository
import Git.Types hiding (Object)
import GitmonClient
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Prelude
import Prologue (liftIO)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty

spec :: Spec
spec = parallel $ do
  describe "gitmon" $ do
    it "receives commands in order" $ do
      withRepository lgFactory "test/fixtures/git/examples/all-languages.git" $ do
        (client, server) <- liftIO $ socketPair AF_UNIX Stream defaultProtocol

        object <- parseObjOid (pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' client "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [update, schedule, finish] = infoToCommands info

        liftIO $ shouldBe (commitOid commit) object
        liftIO $ shouldBe update (Just "update")
        liftIO $ shouldBe schedule (Just "schedule")
        liftIO $ shouldBe finish (Just "finish")

infoToCommands :: ByteString -> [Maybe Text]
infoToCommands input = command' . toObject <$> Prelude.take 3 (split '\n' input)
  where
    command' :: Object -> Maybe Text
    command' = parseMaybe (.: "command")

    toObject :: ByteString -> Object
    toObject = fromJust . decode . fromChunks . (: [])

