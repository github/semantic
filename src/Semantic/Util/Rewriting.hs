{-# LANGUAGE ScopedTypeVariables, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-missing-export-lists #-}
module Semantic.Util.Rewriting where

import Prelude hiding (id, readFile, (.))
import Prologue

import           Control.Category
import qualified Data.ByteString.Char8 as BC
import           Text.Show.Pretty (pPrint)

import           Control.Abstract
import           Control.Abstract.Matching
import           Control.Rewriting hiding (fromMatcher, target)
import           Data.Blob
import           Data.History
import qualified Data.Language as Language
import           Data.Machine
import           Data.Machine.Runner
import           Data.Project hiding (readFile)
import           Data.Record
import qualified Data.Source as Source
import qualified Data.Sum as Sum
import qualified Data.Syntax.Literal as Literal
import           Data.Term
import           Language.JSON.PrettyPrint
import           Language.Python.PrettyPrint
import           Language.Ruby.PrettyPrint
import           Matching.Core
import           Parsing.Parser
import           Reprinting.Pipeline
import           Semantic.IO as IO
import           Semantic.Task

testPythonFile = do
  let path = "test/fixtures/python/reprinting/function.py"
  src  <- blobSource <$> readBlobFromPath (File path Language.Python)
  tree <- parseFile miniPythonParser path
  pure (src, tree)

testPythonPipeline = do
  (src, tree) <- testPythonFile
  printToTerm $ runReprinter src printingPython (mark Refactored tree)

testPythonPipeline' = do
  (src, tree) <- testPythonFile
  pure $ runTokenizing src (mark Refactored tree)

testPythonPipeline'' = do
  (src, tree) <- testPythonFile
  pure $ runContextualizing src (mark Refactored tree)

testPythonPipeline''' = do
  (src, tree) <- testPythonFile
  pure $ runTranslating src printingPython (mark Refactored tree)

testRubyFile = do
  let path = "test/fixtures/ruby/reprinting/infix.rb"
  src  <- blobSource <$> readBlobFromPath (File path Language.Ruby)
  tree <- parseFile miniRubyParser path
  pure (src, tree)

testRubyPipeline = do
  (src, tree) <- testRubyFile
  printToTerm $ runReprinter src printingRuby (mark Refactored tree)

testRubyPipeline' = do
  (src, tree) <- testRubyFile
  pure $ runTokenizing src (mark Refactored tree)

testRubyPipeline'' = do
  (src, tree) <- testRubyFile
  pure $ runContextualizing src (mark Refactored tree)

testJSONPipeline = do
  (src, tree) <- testJSONFile
  printToTerm $ runReprinter src defaultJSONPipeline (mark Refactored tree)

printToTerm = either (putStrLn . show) (BC.putStr . Source.sourceBytes)

testJSONFile = do
  let path = "test/fixtures/javascript/reprinting/map.json"
  src  <- blobSource <$> readBlobFromPath (File path Language.JSON)
  tree <- parseFile jsonParser path
  pure (src, tree)

renameKey :: ( Literal.TextElement :< fs
             , Apply Functor fs
             , term ~ Term (Sum fs) (Record (History : fields))
             )
          => Rewrite (env, term) m (Literal.KeyValue term)
renameKey = do
  Literal.KeyValue k v <- id
  guard (projectTerm k == (Just (Literal.TextElement "\"foo\"")))
  new <- modified (Literal.TextElement "\"fooA\"")
  pure (Literal.KeyValue new v)

testRenameKey = do
  (src, tree) <- testJSONFile
  let (Right tagged) = applyPure (somewhere renameKey) () (mark Unmodified tree)
  pPrint tagged
  printToTerm $ runReprinter src defaultJSONPipeline tagged

increaseNumbers :: (Literal.Float :< fs, Apply Functor fs) => Term (Sum fs) (Record (History ': fields)) -> Term (Sum fs) (Record (History ': fields))
increaseNumbers p = case Sum.project (termOut p) of
  Just (Literal.Float t) -> remark Refactored (termIn (termAnnotation p) (inject (Literal.Float (t <> "0"))))
  Nothing                -> Term (fmap increaseNumbers (unTerm p))

addKVPair :: forall effs syntax ann fields term .
  ( Apply Functor syntax
  , Literal.Hash :< syntax
  , Literal.Array :< syntax
  , Literal.TextElement :< syntax
  , Literal.KeyValue :< syntax
  , ann ~ Record (History ': fields)
  , term ~ Term (Sum syntax) ann
  ) =>
  ProcessT (Eff effs) (Either term (term, Literal.Hash term)) term
addKVPair = repeatedly $ do
  t <- await
  Data.Machine.yield (either id injKVPair t)
  where
    injKVPair :: (term, Literal.Hash term) -> term
    injKVPair (origTerm, Literal.Hash xs) =
      remark Refactored (injectTerm ann (Literal.Hash (xs <> [newItem])))
      where
        newItem = termIn ann (inject (Literal.KeyValue k v))
        k = termIn ann (inject (Literal.TextElement "\"added\""))
        v = termIn ann (inject (Literal.Array []))
        ann = termAnnotation origTerm

testAddKVPair = do
  (src, tree) <- testJSONFile
  tagged <- runM $ cata (toAlgebra (fromMatcher matchHash ~> addKVPair)) (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

overwriteFloats :: forall effs syntax ann fields term .
  ( Apply Functor syntax
  , Literal.Float :< syntax
  , ann ~ Record (History ': fields)
  , term ~ Term (Sum syntax) ann
  ) =>
  ProcessT (Eff effs) (Either term (term, Literal.Float term)) term
overwriteFloats = repeatedly $ do
  t <- await
  Data.Machine.yield (either id injFloat t)
  where injFloat :: (term, Literal.Float term) -> term
        injFloat (term, _) = remark Refactored (termIn (termAnnotation term) (inject (Literal.Float "0")))

testOverwriteFloats = do
  (src, tree) <- testJSONFile
  tagged <- runM $ cata (toAlgebra (fromMatcher matchFloat ~> overwriteFloats)) (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

findKV ::
  ( Literal.KeyValue :< syntax
  , Literal.TextElement :< syntax
  , term ~ Term (Sum syntax) ann
  ) =>
  Text -> ProcessT (Eff effs) term (Either term (term, Literal.KeyValue term))
findKV name = fromMatcher (kvMatcher name)

kvMatcher :: forall fs ann term .
  ( Literal.KeyValue :< fs
  , Literal.TextElement :< fs
  , term ~ Term (Sum fs) ann
  ) =>
  Text -> Matcher term (Literal.KeyValue term)
kvMatcher name = matchM projectTerm target <* matchKey where
  matchKey
    = match Literal.key .
        match Literal.textElementContent $
          ensure (== name)

changeKV :: forall effs syntax ann fields term .
  ( Apply Functor syntax
  , Literal.KeyValue :< syntax
  , Literal.Array :< syntax
  , Literal.Float :< syntax
  , ann ~ Record (History ': fields)
  , term ~ Term (Sum syntax) ann
  ) =>
  ProcessT (Eff effs) (Either term (term, Literal.KeyValue term)) term
changeKV = auto $ either id injKV
  where
    injKV :: (term, Literal.KeyValue term) -> term
    injKV (term, Literal.KeyValue k v) = case projectTerm v of
      Just (Literal.Array elems) -> remark Refactored (termIn ann (inject (Literal.KeyValue k (newArray elems))))
      _                          -> term
      where newArray xs = termIn ann (inject (Literal.Array (xs <> [float])))
            float = termIn ann (inject (Literal.Float "4"))
            ann = termAnnotation term

testChangeKV = do
  (src, tree) <- testJSONFile
  tagged <- runM $ cata (toAlgebra (findKV "\"bar\"" ~> changeKV)) (mark Unmodified tree)
  printToTerm $ runReprinter src defaultJSONPipeline tagged

-- Temporary, until new KURE system lands.
fromMatcher :: Matcher from to -> ProcessT (Eff effs) from (Either from (from, to))
fromMatcher m = auto go where go x = maybe (Left x) (\y -> Right (x, y)) (stepMatcher x m)

-- Turn a 'ProccessT' into an FAlgebra.
toAlgebra :: (Traversable (Base t), Corecursive t)
          => ProcessT (Eff effs) t t
          -> FAlgebra (Base t) (Eff effs t)
toAlgebra m t = do
  inner <- sequenceA t
  res <- runT1 (source (Just (embed inner)) ~> m)
  pure (fromMaybe (embed inner) res)

parseFile :: Parser term -> FilePath -> IO term
parseFile parser = runTask . (parse parser <=< readBlob . file)
