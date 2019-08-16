module Directive ( Directive (..)
                 , parseDirective
                 , toProcess
                 ) where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.Coerce
import           System.Process
import qualified Text.Trifecta as Trifecta

{- | Directives are parsed from magic comments in test files and describe to the test suite how to query the results of a given test case. A directive that looks like this:

@
  # CHECK-JQ: has("mach")
@

would, after converting the contents of the file to a Core expression, dump that expression to JSON and pipe said JSON to @jq -e 'has("mach")@, which will return an error code unless the passed JSON is a hash containing the @"mach"@ key.

This syntax was inspired by LLVM's [FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html). This approach is less direct than tests that pattern-match over an AST, but enable us to keep the text of test cases in close proximity to the assertions we want to make, which improves maintainability significantly and has been a successful strategy for the LLVM and Rust projects.
-}
data Directive = JQ ByteString -- | @# CHECK-JQ: expr@
               | Fails -- | @# CHECK-FAILS@ fails unless translation fails.
                 deriving (Eq, Show)

fails :: Trifecta.Parser Directive
fails = Fails <$ Trifecta.string "# CHECK-FAILS"

jq :: Trifecta.Parser Directive
jq = do
  Trifecta.string "# CHECK-JQ: "
  JQ . ByteString.pack <$> many (Trifecta.noneOf "\n")

directive :: Trifecta.Parser Directive
directive = fails <|> jq

parseDirective :: ByteString -> Either String Directive
parseDirective = Trifecta.foldResult (Left . show) Right
               . Trifecta.parseByteString (directive <* Trifecta.eof) mempty

toProcess :: Directive -> CreateProcess
toProcess (JQ d) = proc "jq" ["-e", ByteString.unpack d]
toProcess x      = error ("can't call toProcess on " <> show x)
