{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Semantic.Stat.Spec (testTree) where

import Control.Exception
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Semantic.Telemetry.Stat
import Semantic.Config
import System.Environment

import Test.Tasty
import Test.Tasty.HUnit

withSocketPair :: ((Socket, Socket) -> IO c) -> IO c
withSocketPair = bracket create release
  where create = socketPair AF_UNIX Datagram defaultProtocol
        release (client, server) = close client >> close server

withEnvironment :: String -> String -> IO () -> IO ()
withEnvironment key value = bracket (setEnv key value) (const (unsetEnv key)) . const

-- NOTE: These cannot easily run in parallel because we test things like
-- setting/unsetting the environment.
testTree :: TestTree
testTree = testCaseSteps "Semantic.Stat.Spec" $ \step -> do
  step "Sets appropriate defaults"
  case_sets_appropriate_defaults
  step "Takes stats addr from environment"
  case_takes_stats_addr_from_environment
  step "Handles stats addr with just hostname"
  case_handles_stats_addr_with_just_hostname
  step "takes dogstats host from environment"
  case_takes_dogstats_host_from_environment
  step "rendering"
  case_render_counters *> case_render_tags
  step "stats deliver datagram"
  case_sendstat_delivers_datagram


case_sets_appropriate_defaults :: Assertion
case_sets_appropriate_defaults = do
  StatsClient{..} <- defaultStatsClient
  statsClientNamespace @?= "semantic"
  statsClientUDPHost   @?= "127.0.0.1"
  statsClientUDPPort   @?= "28125"

case_takes_stats_addr_from_environment :: Assertion
case_takes_stats_addr_from_environment =
  withEnvironment "STATS_ADDR" "localhost:8125" $ do
    StatsClient{..} <- defaultStatsClient
    statsClientUDPHost @?= "localhost"
    statsClientUDPPort @?= "8125"

case_handles_stats_addr_with_just_hostname :: Assertion
case_handles_stats_addr_with_just_hostname =
  withEnvironment "STATS_ADDR" "localhost" $ do
    StatsClient{..} <- defaultStatsClient
    statsClientUDPHost @?= "localhost"
    statsClientUDPPort @?= "28125"

case_takes_dogstats_host_from_environment :: Assertion
case_takes_dogstats_host_from_environment =
  withEnvironment "DOGSTATSD_HOST" "0.0.0.0" $ do
    StatsClient{..} <- defaultStatsClient
    statsClientUDPHost @?= "0.0.0.0"
    statsClientUDPPort @?= "28125"

key :: String
key = "app.metric"

case_render_counters :: Assertion
case_render_counters = do
  renderDatagram "" (increment key []) @?= "app.metric:1|c"
  renderDatagram "" (decrement key []) @?= "app.metric:-1|c"
  renderDatagram "" (count key 8 []) @?= "app.metric:8|c"
  renderDatagram "pre" (increment key []) @?= "pre.app.metric:1|c"

case_render_tags :: Assertion
case_render_tags = do
  let incTag = increment key [("key", "value")]
  renderDatagram "" incTag @?= "app.metric:1|c|#key:value"

  let tagWithoutValue = increment key [("a", "")]
  renderDatagram "" tagWithoutValue @?= "app.metric:1|c|#a"

  let tags = increment key [("key", "value"), ("a", "true")]
  renderDatagram "" tags @?= "app.metric:1|c|#key:value,a:true"

  let tagsWithoutValue = increment key [("key", "value"), ("a", "")]
  renderDatagram "" tagsWithoutValue @?= "app.metric:1|c|#key:value,a"

case_sendstat_delivers_datagram :: Assertion
case_sendstat_delivers_datagram = do
  client@StatsClient{..} <- defaultStatsClient
  withSocketPair $ \(clientSoc, serverSoc) -> do
    sendStat client { statsClientUDPSocket = clientSoc } (increment "app.metric" [])
    info <- recv serverSoc 1024
    info @?= "semantic.app.metric:1|c"

-- Defaults are all driven by defaultConfig.
defaultStatsClient :: IO StatsClient
defaultStatsClient = defaultConfig defaultOptions >>= \Config{..} -> statsClient configStatsHost configStatsPort configAppName
