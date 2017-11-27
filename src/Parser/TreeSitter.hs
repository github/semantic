{-# LANGUAGE DataKinds, ScopedTypeVariables, TypeOperators #-}
module Parser.TreeSitter
( treeSitterParser
, parseToAST
) where

import qualified Assigning.Assignment as A
import Category
import Control.Exception
import Control.Monad ((<=<))
import Data.Blob
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Foldable (toList)
import Data.Functor.Foldable hiding (Nil)
import Data.Range
import Data.Record
import Data.Source
import Data.Span
import Data.Term
import Data.Text (Text, pack)
import Language
import qualified Language.Go as Go
import Foreign
import Foreign.C.String (peekCString)
import Foreign.Marshal.Array (allocaArray)
import qualified Syntax as S
import qualified TreeSitter.Document as TS
import qualified TreeSitter.Node as TS
import qualified TreeSitter.Language as TS
import qualified TreeSitter.Go as TS
import Info

-- | Returns a TreeSitter parser for the given language and TreeSitter grammar.
treeSitterParser :: Ptr TS.Language -> Blob -> IO (Term S.Syntax (Record DefaultFields))
treeSitterParser language blob = bracket TS.ts_document_new TS.ts_document_free $ \ document -> do
  TS.ts_document_set_language document language
  unsafeUseAsCStringLen (sourceBytes (blobSource blob)) $ \ (sourceBytes, len) -> do
    TS.ts_document_set_input_string_with_length document sourceBytes len
    TS.ts_document_parse_halt_on_error document
    term <- documentToTerm language document blob
    pure term


-- | Parse 'Source' with the given 'TS.Language' and return its AST.
parseToAST :: (Bounded grammar, Enum grammar) => Ptr TS.Language -> Blob -> IO (A.AST [] grammar)
parseToAST language Blob{..} = bracket TS.ts_document_new TS.ts_document_free $ \ document -> do
  TS.ts_document_set_language document language
  root <- unsafeUseAsCStringLen (sourceBytes blobSource) $ \ (source, len) -> do
    TS.ts_document_set_input_string_with_length document source len
    TS.ts_document_parse_halt_on_error document
    alloca (\ rootPtr -> do
      TS.ts_document_root_node_p document rootPtr
      peek rootPtr)

  anaM toAST root

toAST :: forall grammar . (Bounded grammar, Enum grammar) => TS.Node -> IO (Base (A.AST [] grammar) TS.Node)
toAST node@TS.Node{..} = do
  let count = fromIntegral nodeChildCount
  children <- allocaArray count $ \ childNodesPtr -> do
    _ <- with nodeTSNode (\ nodePtr -> TS.ts_node_copy_child_nodes nullPtr nodePtr childNodesPtr (fromIntegral count))
    peekArray count childNodesPtr
  pure $! In (A.Node (toEnum (min (fromIntegral nodeSymbol) (fromEnum (maxBound :: grammar)))) (nodeRange node) (nodeSpan node)) children

anaM :: (Corecursive t, Monad m, Traversable (Base t)) => (a -> m (Base t a)) -> a -> m t
anaM g = a where a = pure . embed <=< traverse a <=< g


-- | Return a parser for a tree sitter language & document.
documentToTerm :: Ptr TS.Language -> Ptr TS.Document -> Blob -> IO (Term S.Syntax (Record DefaultFields))
documentToTerm language document Blob{..} = do
  root <- alloca (\ rootPtr -> do
    TS.ts_document_root_node_p document rootPtr
    peek rootPtr)
  toTerm root
  where toTerm :: TS.Node -> IO (Term S.Syntax (Record DefaultFields))
        toTerm node@TS.Node{..} = do
          name <- peekCString nodeType

          children <- getChildren (fromIntegral nodeNamedChildCount) copyNamed
          let allChildren = getChildren (fromIntegral nodeChildCount) copyAll

          let source = slice (nodeRange node) blobSource
          assignTerm language source (range :. categoryForLanguageProductionName language (pack name) :. nodeSpan node :. Nil) children allChildren
          where getChildren count copy = do
                  nodes <- allocaArray count $ \ childNodesPtr -> do
                    _ <- with nodeTSNode (\ nodePtr -> copy nodePtr childNodesPtr (fromIntegral count))
                    peekArray count childNodesPtr
                  children <- traverse toTerm nodes
                  return $! filter isNonEmpty children
                range = nodeRange node
        copyNamed = TS.ts_node_copy_named_child_nodes document
        copyAll = TS.ts_node_copy_child_nodes document

isNonEmpty :: HasField fields Category => Term S.Syntax (Record fields) -> Bool
isNonEmpty = (/= Empty) . category . termAnnotation

nodeRange :: TS.Node -> Range
nodeRange TS.Node{..} = Range (fromIntegral nodeStartByte) (fromIntegral nodeEndByte)

nodeSpan :: TS.Node -> Span
nodeSpan TS.Node{..} = nodeStartPoint `seq` nodeEndPoint `seq` Span (pointPos nodeStartPoint) (pointPos nodeEndPoint)
  where pointPos TS.TSPoint{..} = pointRow `seq` pointColumn `seq` Pos (1 + fromIntegral pointRow) (1 + fromIntegral pointColumn)

assignTerm :: Ptr TS.Language -> Source -> Record DefaultFields -> [ Term S.Syntax (Record DefaultFields) ] -> IO [ Term S.Syntax (Record DefaultFields) ] -> IO (Term S.Syntax (Record DefaultFields))
assignTerm language source annotation children allChildren =
  case assignTermByLanguage source (category annotation) children of
    Just a -> pure (termIn annotation a)
    _ -> defaultTermAssignment source annotation children allChildren
  where assignTermByLanguage :: Source -> Category -> [ Term S.Syntax (Record DefaultFields) ] -> Maybe (S.Syntax (Term S.Syntax (Record DefaultFields)))
        assignTermByLanguage = case languageForTSLanguage language of
          Just Language.Go -> Go.termAssignment
          _ -> \ _ _ _ -> Nothing

defaultTermAssignment :: Source -> Record DefaultFields -> [ Term S.Syntax (Record DefaultFields) ] -> IO [ Term S.Syntax (Record DefaultFields) ] -> IO (Term S.Syntax (Record DefaultFields))
defaultTermAssignment source annotation children allChildren
  | category annotation `elem` operatorCategories = Term . In annotation . S.Operator <$> allChildren
  | otherwise = case (category annotation, children) of
    (ParseError, children) -> toTerm $ S.ParseError children

    (Comment, _) -> toTerm $ S.Comment (toText source)

    (Pair, [key, value]) -> toTerm $ S.Pair key value

    -- Control flow statements
    (If, condition : body) -> toTerm $ S.If condition body
    (Switch, _) -> let (subject, body) = break ((== Other "switch_body") . Info.category . termAnnotation) children in toTerm $ S.Switch subject (body >>= toList . termOut)
    (Case, expr : body) -> toTerm $ S.Case expr body
    (While, expr : rest) -> toTerm $ S.While expr rest

    -- Statements
    (Return, _) -> toTerm $ S.Return children
    (Yield, _) -> toTerm $ S.Yield children
    (Throw, [expr]) -> toTerm $ S.Throw expr
    (Break, [label]) -> toTerm $ S.Break (Just label)
    (Break, []) -> toTerm $ S.Break Nothing
    (Continue, [label]) -> toTerm $ S.Continue (Just label)
    (Continue, []) -> toTerm $ S.Continue Nothing

    (ParenthesizedExpression, [child]) -> pure child

    (Other "unary_expression", _) -> do
      cs <- allChildren
      let c = case category . termAnnotation <$> cs of
                [Other s, _]
                  | s `elem` ["-", "+", "++", "--"] -> MathOperator
                  | s == "~" -> BitwiseOperator
                  | s == "!" -> BooleanOperator
                [_, Other t]
                  | t `elem` ["--", "++"] -> MathOperator
                _ -> Operator
      pure (termIn (setCategory annotation c) (S.Operator cs))

    (Other "binary_expression", _) -> do
      cs <- allChildren
      let c = case category . termAnnotation <$> cs of
                [_, Other s, _]
                  | s `elem` ["<=", "<", ">=", ">", "==", "===", "!=", "!=="] -> RelationalOperator
                  | s `elem` ["*", "+", "-", "/", "%"] -> MathOperator
                  | s `elem` ["&&", "||"] -> BooleanOperator
                  | s `elem` [">>", ">>=", ">>>", ">>>=", "<<", "<<=", "&", "^", "|"] -> BitwiseOperator
                _ -> Operator
      pure (termIn (setCategory annotation c) (S.Operator cs))

    (_, []) -> toTerm $ S.Leaf (toText source)
    (_, children) -> toTerm $ S.Indexed children
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
        toTerm = pure . Term . In annotation


categoryForLanguageProductionName :: Ptr TS.Language -> Text -> Category
categoryForLanguageProductionName = withDefaults . byLanguage
  where
    withDefaults productionMap name = case name of
      "ERROR" -> ParseError
      s -> productionMap s

    byLanguage language = case languageForTSLanguage language of
      Just Language.Go -> Go.categoryForGoName
      _ -> Other


languageForTSLanguage :: Ptr TS.Language -> Maybe Language
languageForTSLanguage = flip lookup
  [ (TS.tree_sitter_go, Language.Go)
  ]
