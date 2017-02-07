{-# LANGUAGE DataKinds, RankNTypes, TypeOperators #-}
module Diffing where

import Prologue hiding (fst, snd)
import Category
import Data.Functor.Both
import Data.RandomWalkSimilarity (defaultFeatureVectorDecorator, stripDiff)
import Data.Record
import qualified Data.Text.IO as TextIO
import Data.These
import Diff
import Info
import Interpreter
import Patch
import Parser
import Renderer
import Renderer.JSON
import Renderer.Patch
import Renderer.Split
import Renderer.Summary
import Renderer.SExpression
import Renderer.TOC
import Source
import Syntax
import System.Directory
import System.FilePath
import qualified System.IO as IO
import System.Environment (lookupEnv)
import Term
import Data.Aeson (ToJSON, toJSON, toEncoding)
import Data.Aeson.Encoding (encodingToLazyByteString)

-- | Given a parser and renderer, diff two sources and return the rendered
-- | result.
-- | Returns the rendered result strictly, so it's always fully evaluated
-- | with respect to other IO actions.
diffFiles :: (HasField fields Category, HasField fields Cost)
          => Parser (Syntax Text) (Record fields)
          -> Renderer (Record fields)
          -> Both SourceBlob
          -> IO Output
diffFiles parse render sourceBlobs = do
  terms <- traverse (fmap (defaultFeatureVectorDecorator getLabel) . parse) sourceBlobs
  pure $! render sourceBlobs (stripDiff (diffTerms' terms))

  where
    diffTerms' terms = case runBothWith areNullOids sourceBlobs of
        (True, False) -> pure $ Insert (snd terms)
        (False, True) -> pure $ Delete (fst terms)
        (_, _) ->
          runBothWith (diffTerms construct compareCategoryEq diffCostWithCachedTermCosts) terms
    areNullOids a b = (hasNullOid a, hasNullOid b)
    hasNullOid blob = oid blob == nullOid || null (source blob)
    construct (info :< syntax) = free (Free ((setCost <$> info <*> sumCost syntax) :< syntax))
    sumCost = fmap getSum . foldMap (fmap Sum . getCost)
    getCost diff = case runFree diff of
      Free (info :< _) -> cost <$> info
      Pure patch -> uncurry both (fromThese 0 0 (unPatch (cost . extract <$> patch)))

getLabel :: HasField fields Category => CofreeF (Syntax leaf) (Record fields) b -> (Category, Maybe leaf)
getLabel (h :< t) = (category h, case t of
  Leaf s -> Just s
  _ -> Nothing)

-- | Determine whether two terms are comparable based on the equality of their categories.
compareCategoryEq :: Functor f => HasField fields Category => Term f (Record fields) -> Term f (Record fields) -> Bool
compareCategoryEq = (==) `on` category . extract

-- | The sum of the node count of the diff’s patches.
diffCostWithCachedTermCosts :: Functor f => HasField fields Cost => Diff f (Record fields) -> Int
diffCostWithCachedTermCosts diff = unCost $ case runFree diff of
  Free (info :< _) -> sum (cost <$> info)
  Pure patch -> sum (cost . extract <$> patch)

-- | Returns a rendered diff given a parser, diff arguments and two source blobs.
textDiff :: (ToJSON (Record fields), DefaultFields fields, HasField fields Cost) => Parser (Syntax Text) (Record fields) -> DiffArguments -> Both SourceBlob -> IO Output
textDiff parser arguments = diffFiles parser $ case format arguments of
  Split -> split
  Patch -> patch
  SExpression -> sExpression
  JSON -> json
  Summary -> summary
  TOC -> toc

-- | Returns a truncated diff given diff arguments and two source blobs.
truncatedDiff :: DiffArguments -> Both SourceBlob -> IO Output
truncatedDiff arguments sources = pure $ case format arguments of
  Split -> SplitOutput mempty
  Patch -> PatchOutput (truncatePatch arguments sources)
  SExpression -> SExpressionOutput mempty
  JSON -> JSONOutput mempty
  Summary -> SummaryOutput mempty
  TOC -> TOCOutput mempty

-- | Prints a rendered diff to stdio or a filepath given a parser, diff arguments and two source blobs.
printDiff :: (ToJSON (Record fields), DefaultFields fields, HasField fields Cost) => Parser (Syntax Text) (Record fields) -> DiffArguments -> Both SourceBlob -> IO ()
printDiff parser arguments sources = do
  rendered <- textDiff parser arguments sources
  writeToOutput (output arguments) $
    case rendered of
      SplitOutput text -> text
      PatchOutput text -> text
      SExpressionOutput text -> text
      JSONOutput series -> encodingToText (toJSON series)
      SummaryOutput summaries -> encodingToText (toJSON summaries)
      TOCOutput summaries -> encodingToText (toJSON summaries)
  where
    -- TODO: Don't go from Value to Text?
    encodingToText = toS . encodingToLazyByteString . toEncoding

-- | Writes text to an output file or stdout.
writeToOutput :: Maybe FilePath -> Text -> IO ()
writeToOutput output text =
  case output of
    Nothing -> do
      lang <- lookupEnv "LANG"
      if isNothing lang then IO.hSetEncoding IO.stdout IO.utf8 else pure ()
      TextIO.hPutStrLn IO.stdout text
    Just path -> do
      isDir <- doesDirectoryExist path
      let outputPath = if isDir
          then path </> (takeFileName outputPath -<.> ".html")
          else path
      IO.withFile outputPath IO.WriteMode (`TextIO.hPutStr` text)
