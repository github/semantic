module GitmonClientSpec where

import Control.Exception
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Foldable
import Data.HashMap.Lazy (empty)
import Data.Maybe (fromMaybe)
import Data.Text hiding (empty)
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
import Text.Regex

spec :: Spec
spec =
  describe "gitmon" $ do
    let wd = "test/fixtures/git/examples/all-languages.git"

    it "receives commands in order" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ sendAll server "continue"
        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, scheduleData, finishData] = infoToCommands info

        liftIO $ do
          shouldBe (commitOid commit) object
          shouldBe updateData (Just "update")
          shouldBe scheduleData (Just "schedule")
          shouldBe finishData (Just "finish")

    it "receives update command with correct data" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ do
          setEnv "GIT_DIR" wd
          setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
          setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"
          setEnv "GIT_SOCKSTAT_VAR_repo_id" "uint:10"
          setEnv "GIT_SOCKSTAT_VAR_user_id" "uint:20"
          sendAll server "continue"

        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, finishData] = infoToData info

        liftIO $ do
          shouldBe (commitOid commit) object
          shouldBe (either id gitDir updateData) wd
          shouldBe (either id program updateData) "cat-file"
          shouldBe (either Just realIP updateData) (Just "127.0.0.1")
          shouldBe (either Just repoName updateData) (Just "examples/all-languages")
          shouldBe (either (const $ Just 1) repoID updateData) (Just 10)
          shouldBe (either (const $ Just 1) userID updateData) (Just 20)
          shouldBe (either id via updateData) "semantic-diff"

          shouldSatisfy (either (const (-1)) cpu finishData) (>= 0)
          shouldSatisfy (either (const (-1)) diskReadBytes finishData) (>= 0)
          shouldSatisfy (either (const (-1)) diskWriteBytes finishData) (>= 0)
          shouldSatisfy (either (const (-1)) resultCode finishData) (>= 0)

    it "reads Nothing for user_id and repo_id when valid prefix but invalid value" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ do
          setEnv "GIT_DIR" wd
          setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
          setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"
          setEnv "GIT_SOCKSTAT_VAR_repo_id" "uint:not_valid"
          setEnv "GIT_SOCKSTAT_VAR_user_id" "uint:not_valid"
          sendAll server "continue"

        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        _ <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, _] = infoToData info

        liftIO $ do
          shouldBe (either (const $ Just 1) repoID updateData) Nothing
          shouldBe (either (const $ Just 1) userID updateData) Nothing

    it "reads Nothing for user_id and repo_id when valid prefix but value is preceeded by invalid chars" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ do
          setEnv "GIT_DIR" wd
          setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
          setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"
          setEnv "GIT_SOCKSTAT_VAR_repo_id" "uint:abc100"
          setEnv "GIT_SOCKSTAT_VAR_user_id" "uint:abc100"
          sendAll server "continue"

        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        _ <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, _] = infoToData info

        liftIO $ do
          shouldBe (either (const $ Just 1) repoID updateData) Nothing
          shouldBe (either (const $ Just 1) userID updateData) Nothing

    it "reads Nothing for user_id and repo_id when valid prefix but value is proceeded by invalid chars" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ do
          setEnv "GIT_DIR" wd
          setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
          setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"
          setEnv "GIT_SOCKSTAT_VAR_repo_id" "uint:100abc"
          setEnv "GIT_SOCKSTAT_VAR_user_id" "uint:100abc"
          sendAll server "continue"

        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        _ <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, _] = infoToData info

        liftIO $ do
          shouldBe (either (const $ Just 1) repoID updateData) Nothing
          shouldBe (either (const $ Just 1) userID updateData) Nothing

    it "reads Nothing for user_id and repo_id when missing prefix but value is valid" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ do
          setEnv "GIT_DIR" wd
          setEnv "GIT_SOCKSTAT_VAR_real_ip" "127.0.0.1"
          setEnv "GIT_SOCKSTAT_VAR_repo_name" "examples/all-languages"
          setEnv "GIT_SOCKSTAT_VAR_repo_id" "100"
          setEnv "GIT_SOCKSTAT_VAR_user_id" "100"
          sendAll server "continue"

        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        _ <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        let [updateData, _, _] = infoToData info

        liftIO $ do
          shouldBe (either (const $ Just 1) repoID updateData) Nothing
          shouldBe (either (const $ Just 1) userID updateData) Nothing

    it "returns the correct git result if the socket is unavailable" . withSocketPair $ \(client, server, socketFactory) ->
      withRepository lgFactory wd $ do
        liftIO $ close client

        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")
        commit <- reportGitmon' socketFactory "cat-file" $ lookupCommit object
        info <- liftIO $ recv server 1024

        liftIO $ shouldBe (commitOid commit) object
        liftIO $ shouldBe "" info

    it "throws if schedule response is fail" . withSocketPair $ \(_, server, socketFactory) ->
      withRepository lgFactory wd $ do
        repo <- getRepository
        liftIO $ sendAll server "fail too busy"
        object <- parseObjOid (Data.Text.pack "dfac8fd681b0749af137aebf3203e77a06fbafc2")

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
infoToCommands input = command' . toObject <$> extract regex input
  where
    command' :: Object -> Maybe Text
    command' = parseMaybe (.: "command")

infoToData :: ByteString -> [Either String ProcessData]
infoToData input = data' . toObject <$> extract regex input
  where
    data' = parseEither parser
    parser o = do
      dataO <- o .: "data"
      asum [ ProcessUpdateData <$> (dataO .: "git_dir") <*> (dataO .: "program") <*> (dataO .:? "real_ip") <*> (dataO .:? "repo_name") <*> (dataO .:? "repo_id") <*> (dataO .:? "user_id") <*> (dataO .: "via")
           , ProcessFinishData <$> (dataO .: "cpu") <*> (dataO .: "disk_read_bytes") <*> (dataO .: "disk_write_bytes") <*> (dataO .: "result_code")
           , pure ProcessScheduleData
           ]

toObject :: ByteString -> Object
toObject input = fromMaybe empty (decodeStrict input)

regex :: Regex
regex = mkRegexWithOpts "(\\{.*\"update\".*\"\\}\\})(\\{.*\"schedule\"\\})(\\{.*\"finish\".*\\}\\})" False True

extract :: Regex -> ByteString -> [ByteString]
extract regex input = Data.ByteString.Char8.pack <$> fromMaybe [""] (matchRegex regex (Data.ByteString.Char8.unpack input))
