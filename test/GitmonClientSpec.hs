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
import Prelude hiding (lookup)
import Prologue (liftIO, Map)
import System.Environment (setEnv)
import Test.Hspec hiding (shouldBe)
import Test.Hspec.Expectations.Pretty
import Data.Map.Lazy (lookup, fromList)

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

    it "receives update command with correct data" $ do
      let wd = "test/fixtures/git/examples/all-languages.git"
      withRepository lgFactory wd $ do
        liftIO $ setEnv "GIT_DIR" wd
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_user_id" "1"
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_repo_id" "2"
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"

        (client, server) <- liftIO $ socketPair AF_UNIX Stream defaultProtocol

        object <- parseObjOid (pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' client "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, _] = fromJust <$> infoToData info

        liftIO $ shouldBe (commitOid commit) object
        liftIO $ shouldBe (lookup ("git_dir" :: Text) updateData) (Just (pack wd))
        liftIO $ shouldBe (lookup ("program" :: Text) updateData) (Just "cat-file")
        liftIO $ shouldBe (lookup ("real_ip" :: Text) updateData) (Just "127.0.0.1")
        liftIO $ shouldBe (lookup ("repo_id" :: Text) updateData) (Just "2")
        liftIO $ shouldBe (lookup ("repo_name" :: Text) updateData) (Just "examples/all-languages")
        liftIO $ shouldBe (lookup ("user_id" :: Text) updateData) (Just "1")
        liftIO $ shouldBe (lookup ("via" :: Text) updateData) (Just "semantic-diff")

infoToCommands :: ByteString -> [Maybe Text]
infoToCommands input = command' . toObject <$> Prelude.take 3 (split '\n' input)
  where
    command' :: Object -> Maybe Text
    command' = parseMaybe (.: "command")

infoToData :: ByteString -> [Maybe (Map Text Text)]
infoToData input = data' . toObject <$> Prelude.take 3 (split '\n' input)
  where
    data' :: Object -> Maybe (Map Text Text)
    data' = parseMaybe (.: "data")

toObject :: ByteString -> Object
toObject = fromJust . decodeStrict
