{-# LANGUAGE DataKinds, ScopedTypeVariables #-}
module TreeSitter
( treeSitterParser
, parseToAST
, defaultTermAssignment
) where

import Prologue hiding (Constructor)
import Category
import Data.Functor.Foldable hiding (Nil)
import Data.Ix
import Data.Record
import qualified Data.Syntax.Assignment as A
import Language
import qualified Language.C as C
import qualified Language.Go as Go
import qualified Language.TypeScript as TS
import qualified Language.Ruby as Ruby
import Range
import Source
import qualified Syntax
import Foreign
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (allocaArray)
import Data.Text.Foreign (withCStringLen)
import qualified Syntax as S
import Term
import Text.Parser.TreeSitter hiding (Language(..))
import qualified Text.Parser.TreeSitter as TS
import SourceSpan
import Info

-- | Returns a TreeSitter parser for the given language and TreeSitter grammar.
treeSitterParser :: Language -> Ptr TS.Language -> Source -> IO (Term (Syntax.Syntax Text) (Record DefaultFields))
treeSitterParser language grammar source = bracket ts_document_new ts_document_free $ \ document -> do
  ts_document_set_language document grammar
  withCStringLen (toText source) $ \ (sourceText, len) -> do
    ts_document_set_input_string_with_length document sourceText len
    ts_document_parse_halt_on_error document
    term <- documentToTerm language document source
    pure term


-- | Parse 'Source' with the given 'TS.Language' and return its AST.
parseToAST :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Source -> IO (A.AST grammar)
parseToAST language source = bracket ts_document_new ts_document_free $ \ document -> do
  ts_document_set_language document language
  root <- withCStringLen (toText source) $ \ (source, len) -> do
    ts_document_set_input_string_with_length document source len
    ts_document_parse_halt_on_error document
    alloca (\ rootPtr -> do
      ts_document_root_node_p document rootPtr
      peek rootPtr)

  anaM toAST root

toAST :: (Bounded grammar, Enum grammar) => Node -> IO (Base (A.AST grammar) Node)
toAST node@Node{..} = do
  let count = fromIntegral nodeChildCount
  children <- allocaArray count $ \ childNodesPtr -> do
    _ <- with nodeTSNode (\ nodePtr -> ts_node_copy_child_nodes nullPtr nodePtr childNodesPtr (fromIntegral count))
    peekArray count childNodesPtr
  pure $ (safeToEnum (fromIntegral nodeSymbol) :. nodeRange node :. nodeSpan node :. Nil) :< children

anaM :: (Corecursive t, Monad m, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g

safeToEnum :: forall n. (Bounded n, Enum n) => Int -> Maybe n
safeToEnum n | (fromEnum (minBound :: n), fromEnum (maxBound :: n)) `inRange` n = Just (toEnum n)
             | otherwise = Nothing


-- | Return a parser for a tree sitter language & document.
documentToTerm :: Language -> Ptr Document -> Source -> IO (Term (Syntax.Syntax Text) (Record DefaultFields))
documentToTerm language document source = do
  root <- alloca (\ rootPtr -> do
    ts_document_root_node_p document rootPtr
    peek rootPtr)
  toTerm root source
  where toTerm :: Node -> Source -> IO (Term (Syntax.Syntax Text) (Record DefaultFields))
        toTerm node source = do
          name <- peekCString (nodeType node)

          children <- getChildren (fromIntegral (nodeNamedChildCount node)) copyNamed
          let allChildren = getChildren (fromIntegral (nodeChildCount node)) copyAll

          assignTerm language source (range :. categoryForLanguageProductionName language (toS name) :. nodeSpan node :. Nil) children allChildren
          where getChildren count copy = do
                  nodes <- allocaArray count $ \ childNodesPtr -> do
                    _ <- with (nodeTSNode node) (\ nodePtr -> copy nodePtr childNodesPtr (fromIntegral count))
                    peekArray count childNodesPtr
                  children <- traverse childNodeToTerm nodes
                  return $! filter isNonEmpty children
                childNodeToTerm childNode = toTerm childNode (slice (offsetRange (nodeRange childNode) (negate (start range))) source)
                range = nodeRange node
        copyNamed = ts_node_copy_named_child_nodes document
        copyAll = ts_node_copy_child_nodes document

isNonEmpty :: HasField fields Category => SyntaxTerm Text fields -> Bool
isNonEmpty = (/= Empty) . category . extract

nodeRange :: Node -> Range
nodeRange Node{..} = Range (fromIntegral nodeStartByte) (fromIntegral nodeEndByte)

nodeSpan :: Node -> SourceSpan
nodeSpan Node{..} = nodeStartPoint `seq` nodeEndPoint `seq` SourceSpan (pointPos nodeStartPoint) (pointPos nodeEndPoint)
  where pointPos TSPoint{..} = pointRow `seq` pointColumn `seq` SourcePos (1 + fromIntegral pointRow) (1 + fromIntegral pointColumn)

assignTerm :: Language -> Source -> Record DefaultFields -> [ SyntaxTerm Text '[ Range, Category, SourceSpan ] ] -> IO [ SyntaxTerm Text '[ Range, Category, SourceSpan ] ] -> IO (SyntaxTerm Text '[ Range, Category, SourceSpan ])
assignTerm language source annotation children allChildren =
  cofree . (annotation :<) <$> case assignTermByLanguage language source (category annotation) children of
    Just a -> pure a
    _ -> defaultTermAssignment source (category annotation) children allChildren
  where assignTermByLanguage :: Language -> Source -> Category -> [ SyntaxTerm Text '[ Range, Category, SourceSpan ] ] -> Maybe (S.Syntax Text (SyntaxTerm Text '[ Range, Category, SourceSpan ]))
        assignTermByLanguage language = case language of
          C -> C.termAssignment
          Language.Go -> Go.termAssignment
          Ruby -> Ruby.termAssignment
          TypeScript -> TS.termAssignment
          _ -> \ _ _ _ -> Nothing

defaultTermAssignment :: Source -> Category -> [ SyntaxTerm Text DefaultFields ] -> IO [ SyntaxTerm Text DefaultFields ] -> IO (S.Syntax Text (SyntaxTerm Text DefaultFields))
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
categoryForLanguageProductionName = withDefaults . byLanguage
  where
    withDefaults productionMap name = case name of
      "ERROR" -> ParseError
      s -> productionMap s

    byLanguage language = case language of
      C -> C.categoryForCProductionName
      Ruby -> Ruby.categoryForRubyName
      Language.Go -> Go.categoryForGoName
      TypeScript -> TS.categoryForTypeScriptName
      _ -> Other
