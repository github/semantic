{-# LANGUAGE OverloadedStrings #-}

module Directive ( Directive (..)
                 , parseDirective
                 , toProcess
                 ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Coerce
import Control.Applicative
import           System.Process
import qualified Text.Trifecta as Trifecta

data Directive = JQ ByteString
               | Fails
                 deriving Eq

instance Show Directive where
  show (JQ d) = ByteString.unpack d
  show Fails = "fails"

fails :: Trifecta.Parser Directive
fails = Fails <$ Trifecta.string "# CHECK-FAILS"

jq :: Trifecta.Parser Directive
jq = do
  Trifecta.string "# CHECK-JQ: "
  JQ . ByteString.pack <$> many (Trifecta.noneOf "\n")

directive :: Trifecta.Parser Directive
directive = Trifecta.choice [fails, jq]

parseDirective :: ByteString -> Either String Directive
parseDirective = Trifecta.foldResult (Left . show) Right
               . Trifecta.parseByteString (directive <* Trifecta.eof) mempty

toProcess :: Directive -> CreateProcess
toProcess (JQ d) = proc "jq" ["-e", ByteString.unpack d]
toProcess x = error ("can't call toProcess on " <> show x)
