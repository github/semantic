{-# LANGUAGE OverloadedStrings #-}

module Directive ( Directive (..)
                 , parseDirective
                 , toProcess
                 ) where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Coerce
import           System.Process
import qualified Text.Trifecta as Trifecta

newtype Directive = JQ ByteString

instance Show Directive where
  show = ByteString.unpack . coerce

directive :: Trifecta.Parser Directive
directive = do
  Trifecta.string "# CHECK-JQ: "
  JQ <$> Trifecta.restOfLine

parseDirective :: ByteString -> Either String Directive
parseDirective = Trifecta.foldResult (Left . show) Right
               . Trifecta.parseByteString (directive <* Trifecta.eof) mempty

toProcess :: Directive -> CreateProcess
toProcess (JQ d) = proc "jq" ["-e", ByteString.unpack d]
