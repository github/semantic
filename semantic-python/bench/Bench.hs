{-# LANGUAGE LambdaCase, TypeOperators #-}

module Main (main) where

import           Analysis.File
import           Analysis.FlowInsensitive (Heap)
import           Analysis.ScopeGraph
import           Control.Algebra
import           Core.Core
import qualified Core.Eval as Eval
import           Core.Name
import qualified Core.Parser
import qualified Data.ByteString as ByteString
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Pretty
import           Gauge
import           Source.Span
import           Syntax.Scope
import           Syntax.Term
import           Syntax.Var
import           System.Exit (exitFailure)
import qualified System.Path as Path
import qualified Text.Trifecta as Trifecta

count :: Term (Ann Span :+: Core) Int -> Int
count = \case
  Var n -> n
  Alg (L (Ann _ a)) -> count a
  Alg (R core) -> case core of
    a :$ b                     -> 1 + count a + count b
    a :>> b                    -> 1 + count a + count b
    (Named _ a) :>>= (Scope x) -> 1 + count a + count (fmap (unVar (const 0) count) x)
    Lam (Named _ (Scope x))    -> 1 + count (fmap (unVar (const 0) count) x)
    Rec (Named _ (Scope x))    -> 1 + count (fmap (unVar (const 0) count) x)
    Unit                       -> 1
    Bool _                     -> 1
    If a b c                   -> 1 + count a + count b + count c
    String _                   -> 1
    Load _                     -> 1
    Record xs                  -> sum (fmap (\(_, a) -> 1 + count a) xs)
    a :. _                     -> 2 + count a
    a :? _                     -> 2 + count a
    a := b                     -> 1 + count a + count b


parsePrelude :: IO (Term (Ann Span :+: Core) Name)
parsePrelude = do
  preludesrc <- ByteString.readFile "semantic-python/src/Prelude.score"
  let ePrelude = Trifecta.parseByteString (Trifecta.spaces *> Core.Parser.core <* Trifecta.eof) mempty preludesrc
  Trifecta.foldResult (\a -> Pretty.putDoc (Trifecta._errDoc a) *> exitFailure) pure ePrelude

graphPrelude :: Term (Ann Span :+: Core) Name -> (Heap Name (ScopeGraph Name), [File (Either (Path.AbsRelFile, Span, String) (ScopeGraph Name))])
graphPrelude t = scopeGraph Eval.eval [File (Path.absRel "<interactive>") (Span (Pos 1 1) (Pos 1 1)) t]

main :: IO ()
main = do
  prel <- parsePrelude
  putStrLn ("Count of terms in prelude: " <> show (count (1 <$ prel)))
  defaultMain
    [ bgroup "prelude"
      [ bench "parse" $ whnfIO parsePrelude
      , bench "graph" $ nf graphPrelude prel
      ]
    ]
