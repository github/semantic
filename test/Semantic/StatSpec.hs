module Semantic.StatSpec where

import Semantic.Stat
import Test.Hspec hiding (shouldBe, shouldNotBe, shouldThrow, errorCall)
import Test.Hspec.Expectations.Pretty
import System.Environment
import Control.Exception
import Network.Socket hiding (recv)
import Network.Socket.ByteString


withSocketPair :: ((Socket, Socket) -> IO c) -> IO c
withSocketPair = bracket create release
  where create = socketPair AF_UNIX Datagram defaultProtocol
        release (client, server) = close client >> close server

withEnvironment :: String -> String -> (() -> IO ()) -> IO ()
withEnvironment key value = bracket (setEnv key value) (const (unsetEnv key))

-- NOTE: These cannot easily run in parallel because we test things like
-- setting/unsetting the environment.
spec :: Spec
spec = do
  describe "defaultStatsClient" $ do
    it "sets appropriate defaults" $ do
      StatsClient{..} <- defaultStatsClient
      namespace `shouldBe` "semantic"
      udpHost `shouldBe` "127.0.0.1"
      udpPort `shouldBe` "28125"

    around (withEnvironment "STATS_ADDR" "localhost:8125") $
      it "takes STATS_ADDR from environment" $ do
        StatsClient{..} <- defaultStatsClient
        udpHost `shouldBe` "localhost"
        udpPort `shouldBe` "8125"

    around (withEnvironment "DOGSTATSD_HOST" "0.0.0.0") $
      it "takes DOGSTATSD_HOST from environment" $ do
        StatsClient{..} <- defaultStatsClient
        udpHost `shouldBe` "0.0.0.0"
        udpPort `shouldBe` "28125"

  describe "renderDatagram" $ do
    let key = "app.metric"

    describe "counters" $ do
      it "renders increment" $
        renderDatagram "" (increment key []) `shouldBe` "app.metric:1|c"
      it "renders decrement" $
        renderDatagram "" (decrement key []) `shouldBe` "app.metric:-1|c"
      it "renders count" $
        renderDatagram "" (count key 8 []) `shouldBe` "app.metric:8|c"

    it "renders namespace" $
      renderDatagram "pre" (increment key []) `shouldBe` "pre.app.metric:1|c"

    describe "tags" $ do
      it "renders a tag" $ do
        let inc = increment key [("key", "value")]
        renderDatagram "" inc `shouldBe` "app.metric:1|c|#key:value"
      it "renders a tag without value" $ do
        let inc = increment key [("a", "")]
        renderDatagram "" inc `shouldBe` "app.metric:1|c|#a"
      it "renders tags" $ do
        let inc = increment key [("key", "value"), ("a", "true")]
        renderDatagram "" inc `shouldBe` "app.metric:1|c|#key:value,a:true"
      it "renders tags without value" $ do
        let inc = increment key [("key", "value"), ("a", "")]
        renderDatagram "" inc `shouldBe` "app.metric:1|c|#key:value,a"

  describe "sendStats" $
    it "delivers datagram" $ do
      client@StatsClient{..} <- defaultStatsClient
      withSocketPair $ \(clientSoc, serverSoc) -> do
        sendStats client { udpSocket = clientSoc } (increment "app.metric" [])
        info <- recv serverSoc 1024
        info `shouldBe` "semantic.app.metric:1|c"
