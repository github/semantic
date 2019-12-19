{-# LANGUAGE TypeOperators #-}

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
import           Gauge
import           Source.Span
import           Syntax.Term
import qualified System.Path as Path
import qualified Text.Trifecta as Trifecta

parsePrelude :: IO (Term (Ann Span :+: Core) Name)
parsePrelude = do
  preludesrc <- ByteString.readFile "semantic-python/src/Prelude.score"
  let ePrelude = Trifecta.parseByteString (Core.Parser.core <* Trifecta.eof) mempty preludesrc
  case Trifecta.foldResult (Left . show) Right ePrelude of
    Right r -> pure r
    Left s  -> fail ("Couldn't parse prelude: " <> s)

graphPrelude :: Term (Ann Span :+: Core) Name -> (Heap Name (ScopeGraph Name), [File (Either (Path.AbsRelFile, Span, String) (ScopeGraph Name))])
graphPrelude t = scopeGraph Eval.eval [File (Path.absRel "<interactive>") (Span (Pos 1 1) (Pos 1 1)) t]

main :: IO ()
main = do
  prel <- parsePrelude
  defaultMain
    [ bgroup "prelude"
      [ bench "parse" $ whnfIO parsePrelude
      , bench "graph" $ nf graphPrelude prel
      ]
    ]
