{-# LANGUAGE TypeApplications, TypeOperators #-}

-- | FileCheck-style directives for testing Core compilers.
module Directive ( Directive (..)
                 , readDirectivesFromFile
                 , describe
                 ) where

import           Analysis.Concrete (Concrete (..))
import           Control.Algebra
import           Control.Monad
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
-- import           Core.Core (Core)
-- import qualified Core.Core as Core
-- import           Core.Name (Name)
-- import qualified Core.Parser
-- import qualified Core.Pretty
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
import qualified Text.Parser.Token.Style as Style
import           Text.Trifecta (CharParsing, TokenParsing (..))
import qualified Text.Trifecta as Trifecta

{- |

Directives are parsed from magic comments in test files and
describe to the test suite how to query the results of a given test
case. A directive that looks like this:

@
  # CHECK-RESULT: key: value
@

would test that the value for @key@ in the result evaluates to the given
concrete value.

This syntax was inspired by LLVM's
[FileCheck](https://llvm.org/docs/CommandGuide/FileCheck.html). This
approach is less direct than tests that pattern-match over an AST, but
enable us to keep the text of test cases in close proximity to the
assertions we want to make, which improves maintainability
significantly and has been a successful strategy for the LLVM and Rust
projects.

-}
data Directive = Tree (Term Core Name) -- | @# CHECK-TREE: core@
               | Result Text (Concrete (Term (Core.Ann Source.Span :+: Core))) -- | @# CHECK-RESULT key: expected
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
describe (Result t e) = T.unpack t <> ": " <> show e

fails :: CharParsing m => m Directive
fails = Fails <$ Trifecta.string "# CHECK-FAILS"

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

concrete :: TokenParsing m => m (Concrete term)
concrete = Trifecta.choice
  [ String <$> Trifecta.stringLiteral
  , Bool True <$ Trifecta.symbol "#true"
  , Bool False <$ Trifecta.symbol "#false"
  , Unit <$ Trifecta.symbol "#unit"
  ]

directive :: (Monad m, TokenParsing m) => m Directive
directive = Trifecta.choice [ fails, result, tree ]

parseDirective :: ByteString -> Either String Directive
parseDirective = Trifecta.foldResult (Left . show) Right
               . Trifecta.parseByteString (directive <* Trifecta.eof) mempty
