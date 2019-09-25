module Directive ( Directive (..)
                 , parseDirective
                 , describe
                 , toProcess
                 ) where

import           Control.Applicative
import           Control.Monad
import           Data.Name (Name)
import           Data.Term (Term)
import           Data.Core (Core)
import qualified Data.Core.Parser as Core.Parser
import qualified Data.Core.Pretty as Core.Pretty
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import           Data.List.NonEmpty (NonEmpty)
import           System.Process
import qualified Text.Trifecta as Trifecta

{- |

Directives are parsed from magic comments in test files and
describe to the test suite how to query the results of a given test
case. A directive that looks like this:

@
  # CHECK-JQ: has("mach")
@

would, after converting the contents of the file to a Core expression,
dump that expression to JSON and pipe said JSON to @jq -e
'has("mach")@, which will return an error code unless the passed JSON
is a hash containing the @"mach"@ key.

This syntax was inspired by LLVM's
[FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html). This
approach is less direct than tests that pattern-match over an AST, but
enable us to keep the text of test cases in close proximity to the
assertions we want to make, which improves maintainability
significantly and has been a successful strategy for the LLVM and Rust
projects.

-}
data Directive = JQ ByteString -- | @# CHECK-JQ: expr@
               | Tree (Term Core Name) -- | @# CHECK-TREE: core@
               | Fails -- | @# CHECK-FAILS@ fails unless translation fails.
                 deriving (Eq, Show)

describe :: Directive -> String
describe Fails = "<expect failure>"
describe (Tree t) =  Core.Pretty.showCore t
describe (JQ b) = ByteString.unpack b

fails :: Trifecta.Parser Directive
fails = Fails <$ Trifecta.string "# CHECK-FAILS"

jq :: Trifecta.Parser Directive
jq = do
  void $ Trifecta.string "# CHECK-JQ: "
  JQ . ByteString.pack <$> many (Trifecta.noneOf "\n")

tree :: Trifecta.Parser Directive
tree = do
  void $ Trifecta.string "# CHECK-TREE: "
  Tree <$> Core.Parser.core

directive :: Trifecta.Parser Directive
directive = Trifecta.choice [ fails, jq, tree ]

parseDirective :: ByteString -> Either String Directive
parseDirective = Trifecta.foldResult (Left . show) Right
               . Trifecta.parseByteString (directive <* Trifecta.eof) mempty

toProcess :: Directive -> CreateProcess
toProcess (JQ d) = proc "jq" ["-e", ByteString.unpack d]
toProcess x      = error ("can't call toProcess on " <> show x)
