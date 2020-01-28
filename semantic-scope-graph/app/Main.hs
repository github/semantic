{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import           Control.Applicative
import qualified Options.Applicative as Opt
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.PartClass

data Config = Config
  { persistTo   :: Maybe Path.AbsRelFile
  , persistFrom :: Maybe Path.AbsRelFile
  , targetPath  :: Path.AbsRelFileDir
  } deriving Show

configParser :: Opt.Parser Config
configParser = do
  mFrom  <- optional (pathOption (Opt.short 'f' <> Opt.long "from" <> Opt.help "Construct the scope graph using the existing graph at FILE"))
  mTo    <- optional (pathOption (Opt.short 't' <> Opt.long "to"   <> Opt.help "Persist the resulting scope graph to FILE"))
  target <- Opt.argument path (Opt.metavar "TARGET" <> Opt.help "Path to which to persist the resulting scope graph")
  pure (Config mFrom mTo target)

path :: (Path.PartClass.FileDir fd) => Opt.ReadM (Path.AbsRel fd)
path = Opt.eitherReader Path.parse

pathOption :: Path.PartClass.FileDir fd => Opt.Mod Opt.OptionFields (Path.AbsRel fd) -> Opt.Parser (Path.AbsRel fd)
pathOption = Opt.option path

main :: IO ()
main = do
  cfg <- Opt.execParser (Opt.info configParser Opt.fullDesc)
  print cfg
