{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Algebra.Graph.Export.Dot (Attribute (..))
import qualified Algebra.Graph.Export.Dot as Dot
import           Analysis.Name (Name, formatName, nameI)
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
import qualified Scope.Graph.Algebraic as Algebraic
import           Scope.Graph.Convert
import           Scope.Info
import           Scope.Types
import           Source.Span
import           System.Environment
import           System.IO
import qualified System.Path as Path
import           Text.Pretty.Simple

renderSpan :: Span -> Text
renderSpan (Span (Pos a b) (Pos c d)) =
  let t = T.pack . show
  in T.concat ["[", t a, "-", t b, "] - [", t c, "-", t d, "]"]

fashion :: FilePath -> Algebraic.Graph Name -> Dot.Style (Algebraic.Node Name) Text
fashion fp g = Dot.Style
  { Dot.graphName = T.pack . show $ fp
  , Dot.preamble  = []
  , Dot.graphAttributes = []
  , Dot.defaultVertexAttributes = []
  , Dot.defaultEdgeAttributes = []
  , Dot.vertexName = \case
      Algebraic.Node n _ -> formatName n
      Algebraic.Informational i -> formatName (coerce (infoDeclaration i))
      Algebraic.Declares i   -> formatName (coerce (infoDeclaration i)) <> "_decl"
  , Dot.vertexAttributes = \case
      Algebraic.Node n s ->
        [ "label" := formatName n
        , "shape" := "circle"
        ]
      Algebraic.Declares n ->
        [ "label" := formatName (coerce (infoDeclaration n))
        , "shape" := "square"
        , "color" := "gray"
        ]
      Algebraic.Informational i ->
        let
          sections = ["Info", dotName, dotKind]
          dotName = formatName (coerce (infoDeclaration i))
          dotKind = T.pack (show (infoKind i))
        in
          ["label" := T.intercalate "|" sections
          ,"shape" := "record"]

  , Dot.edgeAttributes = \a b -> case Algebraic.edgeLabel a b g of
      Algebraic.Declaring     -> ["color" := "red"]
      Algebraic.Referencing s -> ["color" := "blue", "label" := (T.intercalate "," . fmap (T.pack . show) . sort . toList $ s)]
  }
  where

soNowWeDump :: Data.ScopeGraph.ScopeGraph Name -> IO ()
soNowWeDump = Data.ScopeGraph.foldGraph go (nameI 0)
  where
    go :: Name -> (Data.ScopeGraph.EdgeLabel -> IO ()) -> IO ()
    go n fn = do
      putStrLn "Entering"
      pPrint n
      let go e = pPrint e *> fn e
      traverse_ @[] go [Lexical .. Superclass]

main :: IO ()
main = do
  file:_ <- getArgs
  (graph, res) <- graphPythonFile (Path.absRel file)
  let p :: Show a => a -> IO ()
      p = pHPrint stdout
  case res of
    Complete  -> pure ()
    Todo list -> do
      p @String "Graph conversion was not successful."
      p @String "TODOs remaining:"
      traverse_ p list

  soNowWeDump graph
  p @String "****"
  let asAlg = Algebraic.fromPrimitive graph
  p asAlg
  p @String "****"
  T.putStrLn . Dot.export (fashion file asAlg) $ asAlg
