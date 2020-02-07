{-# LANGUAGE LambdaCase #-}
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
import           Scope.Info
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
  , Dot.vertexName = \case
      Algebraic.Node n _ -> formatName n
      Algebraic.Informational i    -> formatName (unDeclaration (infoDeclaration i))
  , Dot.vertexAttributes = \case
      Algebraic.Node n s ->
        let sections = ["Scope", formatName n]
        in ["label" := T.intercalate "|" sections]
      Algebraic.Informational i ->
        let
          sections = ["Info", dotName, dotKind]
          dotName = formatName (unDeclaration (infoDeclaration i))
          dotKind = T.pack (show (infoKind i))
        in
          ["label" := T.intercalate "|" sections]

  , Dot.edgeAttributes = \a b -> case Algebraic.edgeLabel a b g of
      Algebraic.Strong   -> []
      Algebraic.Parent s -> ["label" := (T.intercalate "," . fmap (T.pack . show) . sort . toList $ s)]
  }
  where


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


