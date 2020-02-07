{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import           Algebra.Graph.Export.Dot (Attribute (..))
import qualified Algebra.Graph.Export.Dot as Dot
import           Analysis.Name (Name, formatName)
import           Control.Lens.Getter
import           Data.Coerce
import           Data.Foldable
import           Data.List (sort)
import qualified Data.ScopeGraph
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Language.Python (graphPythonFile)
import qualified ScopeGraph.Algebraic as Algebraic
import           ScopeGraph.Convert
import           Source.Span
import           System.Environment
import           System.IO
import qualified System.Path as Path

renderSpan :: Span -> Text
renderSpan (Span (Pos a b) (Pos c d)) =
  let t = T.pack . show
  in T.concat ["[", t a, "-", t b, "] - [", t c, "-", t d, "]"]

fashion :: FilePath -> Algebraic.Graph Name -> Dot.Style (Algebraic.Node Name) Text
fashion fp g = Dot.Style
  { Dot.graphName = T.pack fp
  , Dot.preamble  = []
  , Dot.graphAttributes = []
  , Dot.defaultVertexAttributes = ["shape" := "record"]
  , Dot.defaultEdgeAttributes = []
  , Dot.vertexName = \(Algebraic.Node n _) -> formatName n
  , Dot.vertexAttributes = \(Algebraic.Node n s) ->
      let sections = [formatName n, dotName, dotSpan, dotKind]
          decls = Data.ScopeGraph.declarations s
          dotName = case decls of
            [info] -> formatName (coerce (Data.ScopeGraph.infoDeclaration info))
            []     -> "??"
            _      -> "MULTIPLE (" <> T.pack (show (length decls)) <> ")"
          dotSpan = case decls of
            [info] -> renderSpan (info^.span_)
            _      -> ""
          dotKind = case decls of
            [info] -> T.pack (show (Data.ScopeGraph.infoKind info))
            _      -> ""
      in ["label" := T.intercalate "|" sections]

  , Dot.edgeAttributes = \a b -> ["label" := buildLabel (Algebraic.edgeLabels a b g)]
  }
  where
    buildLabel = T.intercalate "," . fmap (T.pack . show) . sort . toList

main :: IO ()
main = do
  file:_ <- getArgs
  (graph, res) <- graphPythonFile (Path.absRel file)
  let p = hPutStrLn stdout
  case res of
    Complete  -> pure ()
    Todo list -> do
      p "Graph conversion was not successful."
      p "TODOs remaining:"
      traverse_ p list

  p (show $ graph)

  let asAlg = Algebraic.fromPrimitive graph
  T.putStrLn . Dot.export (fashion file asAlg) $ asAlg


