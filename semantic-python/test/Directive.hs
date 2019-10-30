{-# LANGUAGE TypeApplications, TypeOperators #-}

-- | FileCheck-style directives for testing Core compilers.
module Directive ( Directive (..)
                 , readDirectivesFromFile
                 , describe
                 , toProcess
                 ) where

import           Analysis.Concrete (Concrete (..))
import           Control.Applicative
import           Control.Effect
import           Control.Monad
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Core.Core (Core)
import qualified Core.Core as Core
import           Core.Name (Name)
import qualified Core.Parser
import qualified Core.Pretty
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Source.Span as Source
import qualified Streaming.Prelude as Stream
import           Syntax.Term (Term)
import qualified System.Path as Path
import qualified System.Path.PartClass as Path.Class
import           System.Process
import qualified Text.Parser.Token.Style as Style
import           Text.Trifecta (CharParsing, TokenParsing (..))
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
               | Result Text (Concrete (Term (Core.Ann Source.Span :+: Core)) Name) -- | @# CHECK-RESULT key: expected
               | Fails -- | @# CHECK-FAILS@ fails unless translation fails.
                 deriving (Eq, Show)

-- | Extract all directives from a file.
readDirectivesFromFile :: Path.Class.AbsRel ar => Path.File ar -> IO [Directive]
readDirectivesFromFile
  = runResourceT
  . Stream.toList_
  . Stream.mapM (either perish pure . parseDirective)
  . Stream.takeWhile isComment
  . Stream.mapped ByteStream.toStrict
  . ByteStream.lines
  . ByteStream.readFile @(ResourceT IO)
  . Path.toString
    where
      perish s  = fail ("Directive parsing error: " <> s)
      isComment = (== Just '#') . fmap fst . ByteString.uncons


describe :: Directive -> String
describe Fails        = "<expect failure>"
describe (Tree t)     =  Core.Pretty.showCore t
describe (JQ b)       = ByteString.unpack b
describe (Result t e) = T.unpack t <> ": " <> show e

fails :: CharParsing m => m Directive
fails = Fails <$ Trifecta.string "# CHECK-FAILS"

jq :: (Monad m, CharParsing m) => m Directive
jq = do
  void $ Trifecta.string "# CHECK-JQ: "
  JQ . ByteString.pack <$> many (Trifecta.noneOf "\n")

tree :: (Monad m, TokenParsing m) => m Directive
tree = do
  void $ Trifecta.string "# CHECK-TREE: "
  Tree <$> Core.Parser.core

result :: (Monad m, TokenParsing m) => m Directive
result = do
  void $ Trifecta.string "# CHECK-RESULT "
  key <- Trifecta.ident Style.haskellIdents
  void $ Trifecta.symbolic ':'
  Result key <$> concrete

concrete :: TokenParsing m => m (Concrete term Name)
concrete = Trifecta.choice
  [ String <$> Trifecta.stringLiteral
  , Bool True <$ Trifecta.symbol "#true"
  , Bool False <$ Trifecta.symbol "#false"
  , Unit <$ Trifecta.symbol "#unit"
  ]

directive :: (Monad m, TokenParsing m) => m Directive
directive = Trifecta.choice [ fails, result, jq, tree ]

parseDirective :: ByteString -> Either String Directive
parseDirective = Trifecta.foldResult (Left . show) Right
               . Trifecta.parseByteString (directive <* Trifecta.eof) mempty

toProcess :: Directive -> CreateProcess
toProcess (JQ d) = proc "jq" ["-e", ByteString.unpack d]
toProcess x      = error ("can't call toProcess on " <> show x)
