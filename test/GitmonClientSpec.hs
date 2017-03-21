module GitmonClientSpec where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (split, ByteString)
import Data.Foldable
import Data.Maybe (fromJust)
import Data.Text hiding (split, take)
import Git.Libgit2
import Git.Repository
import Git.Types hiding (Object)
import GitmonClient
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Prelude hiding (lookup)
import Prologue (liftIO, runReaderT)
import System.Environment (setEnv)
import Test.Hspec hiding (shouldBe, shouldSatisfy, shouldThrow, anyErrorCall)
import Test.Hspec.Expectations.Pretty
import Control.Exception

spec :: Spec
spec = parallel $ do
  describe "gitmon" $ do
    let wd = "test/fixtures/git/examples/all-languages.git"
    it "receives commands in order" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ sendAll server "continue"
        object <- parseObjOid (pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [update, schedule, finish] = infoToCommands info

        liftIO $ shouldBe (commitOid commit) object
        liftIO $ shouldBe update (Just "update")
        liftIO $ shouldBe schedule (Just "schedule")
        liftIO $ shouldBe finish (Just "finish")

    it "receives update command with correct data" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ setEnv "GIT_DIR" wd
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_repo_id" "uint:10"
        liftIO $ setEnv "GIT_SOCKSTAT_VAR_user_id" "uint:20"

        liftIO $ sendAll server "continue"
        object <- parseObjOid (pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, finishData] = infoToData info

        liftIO $ shouldBe (commitOid commit) object
        liftIO $ shouldBe (either id gitDir updateData) wd
        liftIO $ shouldBe (either id program updateData) "cat-file"
        liftIO $ shouldBe (either Just realIP updateData) (Just "127.0.0.1")
        liftIO $ shouldBe (either Just repoName updateData) (Just "examples/all-languages")
        liftIO $ shouldBe (either (const $ Just 1) repoID updateData) (Just 10)
        liftIO $ shouldBe (either (const $ Just 1) userID updateData) (Just 20)
        liftIO $ shouldBe (either id via updateData) "semantic-diff"

        liftIO $ shouldSatisfy (either (const (-1)) cpu finishData) (>= 0)
        liftIO $ shouldSatisfy (either (const (-1)) diskReadBytes finishData) (>= 0)
        liftIO $ shouldSatisfy (either (const (-1)) diskWriteBytes finishData) (>= 0)
        liftIO $ shouldSatisfy (either (const (-1)) resultCode finishData) (>= 0)

    it "returns the correct git result if the socket is unavailable" . withSocketPair $ \(client, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ close client

        object <- parseObjOid (pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        liftIO $ shouldBe (commitOid commit) object
        liftIO $ shouldBe "" info

    it "throws if schedule response is fail" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        repo <- getRepository
        liftIO $ sendAll server "fail too busy"
        object <- parseObjOid (pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")

        liftIO $ shouldThrow (runReaderT (reportGitmon' socketFactory "cat-file" (lookupCommit object)) repo) gitmonException

gitmonException :: GitmonException -> Bool
gitmonException = const True

withSocketPair :: ((Socket, Socket, SocketFactory) -> IO c) -> IO c
withSocketPair = bracket create release
  where
    create = do
      (client, server) <- socketPair AF_UNIX Stream defaultProtocol
      pure (client, server, SocketFactory (\f -> f client))
    release (client, server, _) = do
      close client
      close server

infoToCommands :: ByteString -> [Maybe Text]
infoToCommands input = command' . toObject <$> Prelude.take 3 (split '\n' input)
  where
    command' :: Object -> Maybe Text
    command' = parseMaybe (.: "command")

infoToData :: ByteString -> [Either String ProcessData]
infoToData input = data' . toObject <$> Prelude.take 3 (split '\n' input)
  where data' = parseEither parser
        parser o = do
          dataO <- o .: "data"
          asum [ ProcessUpdateData <$> (dataO .: "git_dir") <*> (dataO .: "program") <*> (dataO .:? "real_ip") <*> (dataO .:? "repo_name") <*> (dataO .:? "repo_id") <*> (dataO .:? "user_id") <*> (dataO .: "via")
               , ProcessFinishData <$> (dataO .: "cpu") <*> (dataO .: "disk_read_bytes") <*> (dataO .: "disk_write_bytes") <*> (dataO .: "result_code")
               , pure ProcessScheduleData
               ]

toObject :: ByteString -> Object
toObject = fromJust . decodeStrict
