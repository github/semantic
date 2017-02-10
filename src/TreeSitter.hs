{-# LANGUAGE DataKinds #-}
module TreeSitter
( treeSitterParser
, defaultTermAssignment
) where

import Prologue hiding (Constructor)
import Category
import Data.Record
import Language
import qualified Language.C as C
import qualified Language.Go as Go
import qualified Language.JavaScript as JS
import qualified Language.Ruby as Ruby
import Parser
import Range
import Source
import qualified Syntax
import Foreign
import Foreign.C.String (peekCString)
import Data.Text.Foreign (withCStringLen)
import qualified Syntax as S
import Term
import Text.Parser.TreeSitter hiding (Language(..))
import qualified Text.Parser.TreeSitter as TS
import SourceSpan
import Info

-- | Returns a TreeSitter parser for the given language and TreeSitter grammar.
treeSitterParser :: Language -> Ptr TS.Language -> Parser (Syntax.Syntax Text) (Record '[Range, Category, SourceSpan])
treeSitterParser language grammar blob = do
  document <- ts_document_new
  ts_document_set_language document grammar
  withCStringLen (toText (source blob)) $ \ (source, len) -> do
    ts_document_set_input_string_with_length document source len
    ts_document_parse document
    term <- documentToTerm language document blob
    ts_document_free document
    pure term


-- | Return a parser for a tree sitter language & document.
documentToTerm :: Language -> Ptr Document -> Parser (Syntax.Syntax Text) (Record '[Range, Category, SourceSpan])
documentToTerm language document SourceBlob{..} = alloca $ \ root -> do
  ts_document_root_node_p document root
  toTerm root (totalRange source) source
  where toTerm node range source = do
          name <- ts_node_p_name node document
          name <- peekCString name
          count <- ts_node_p_named_child_count node
          children <- filter isNonEmpty <$> traverse (alloca . getChild ts_node_p_named_child (start range) node) (take (fromIntegral count) [0..])

          let startPos = SourcePos (1 + (fromIntegral $! ts_node_p_start_point_row node)) (1 + (fromIntegral $! ts_node_p_start_point_column node))
          let endPos = SourcePos (1 + (fromIntegral $! ts_node_p_end_point_row node)) (1 + (fromIntegral $! ts_node_p_end_point_column node))
          let sourceSpan = SourceSpan { spanStart = startPos , spanEnd = endPos }

          allChildrenCount <- ts_node_p_child_count node
          let allChildren = filter isNonEmpty <$> traverse (alloca . getChild ts_node_p_child (start range) node) (take (fromIntegral allChildrenCount) [0..])

          -- Note: The strict application here is semantically important.
          -- Without it, we may not evaluate the value until after we’ve exited
          -- the scope that `node` was allocated within, meaning `alloca` will
          -- free it & other stack data may overwrite it.
          range `seq` sourceSpan `seq` assignTerm language source (range :. categoryForLanguageProductionName language (toS name) :. sourceSpan :. Nil) children allChildren
          where getChild getter node n out = do
                  _ <- getter node n out
                  let childRange = nodeRange node
                  toTerm out childRange (slice childRange source)
                {-# INLINE getChild #-}
        isNonEmpty child = category (extract child) /= Empty

        nodeRange node = Range { start = fromIntegral $ ts_node_p_start_char node, end = fromIntegral $ ts_node_p_end_char node }

assignTerm :: Language -> Source -> Record '[Range, Category, SourceSpan] -> [ SyntaxTerm Text '[ Range, Category, SourceSpan ] ] -> IO [ SyntaxTerm Text '[ Range, Category, SourceSpan ] ] -> IO (SyntaxTerm Text '[ Range, Category, SourceSpan ])
assignTerm language source annotation children allChildren =
  cofree . (annotation :<) <$> case assignTermByLanguage language source (category annotation) children of
    Just a -> pure a
    _ -> defaultTermAssignment source (category annotation) children allChildren
  where assignTermByLanguage :: Language -> Source -> Category -> [ SyntaxTerm Text '[ Range, Category, SourceSpan ] ] -> Maybe (S.Syntax Text (SyntaxTerm Text '[ Range, Category, SourceSpan ]))
        assignTermByLanguage = \case
          JavaScript -> JS.termAssignment
          C -> C.termAssignment
          Language.Go -> Go.termAssignment
          Ruby -> Ruby.termAssignment
          _ -> \ _ _ _ -> Nothing

defaultTermAssignment :: Source -> Category -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -> IO (S.Syntax Text (SyntaxTerm Text '[Range, Category, SourceSpan]))
defaultTermAssignment source category children allChildren
  | category `elem` operatorCategories = S.Operator <$> allChildren
  | otherwise = pure $! case (category, children) of
    (ParseError, children) -> S.ParseError children

    (Comment, _) -> S.Comment (toText source)

    (Pair, [key, value]) -> S.Pair key value

    -- Control flow statements
    (If, condition : body) -> S.If condition body
    (Switch, _) -> uncurry S.Switch (Prologue.break ((== Case) . Info.category . extract) children)
    (Case, expr : body) -> S.Case expr body
    (While, expr : rest) -> S.While expr rest

    -- Statements
    (Return, _) -> S.Return children
    (Yield, _) -> S.Yield children
    (Throw, [expr]) -> S.Throw expr
    (Break, [label]) -> S.Break (Just label)
    (Break, []) -> S.Break Nothing
    (Continue, [label]) -> S.Continue (Just label)
    (Continue, []) -> S.Continue Nothing

    (_, []) -> S.Leaf (toText source)
    (_, children) -> S.Indexed children
  where operatorCategories =
          [ Operator
          , Binary
          , Unary
          , RangeExpression
          , ScopeOperator
          , BooleanOperator
          , MathOperator
          , RelationalOperator
          , BitwiseOperator
          ]


categoryForLanguageProductionName :: Language -> Text -> Category
categoryForLanguageProductionName = withDefaults . \case
  JavaScript -> JS.categoryForJavaScriptProductionName
  C -> C.categoryForCProductionName
  Ruby -> Ruby.categoryForRubyName
  Language.Go -> Go.categoryForGoName
  _ -> Other
  where withDefaults productionMap = \case
          "ERROR" -> ParseError
          s -> productionMap s
