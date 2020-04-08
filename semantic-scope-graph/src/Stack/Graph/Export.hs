{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Stack.Graph.Export
  ( toGraphViz
  , openGraphViz
  ) where

import           Algebra.Graph.Export.Dot (Attribute (..))
import qualified Algebra.Graph.Export.Dot as Dot
import           Analysis.Name
import           Control.Monad
import qualified Data.ByteString.Streaming.Char8 as ByteStream
import qualified Data.Text as T
import           Stack.Graph
import           Streaming
import qualified Streaming.Process
import           System.IO
import           System.Process (system)
import qualified System.Process as Process

sym :: Symbol -> String
sym = T.unpack . formatName

nodeToDotName :: Tagged Node -> String
nodeToDotName (_ :# i) = "node_" <> show i

nodeAttributes :: Tagged Node -> [Dot.Attribute String]
nodeAttributes (node :# idx) = case node of
  Declaration s _ _ -> [ "shape" := "rect", "label" := sym s, "color" := "red", "penwidth" := "5" ]
  Reference s _ _   -> [ "shape" := "rect", "label" := sym s, "color" := "green", "peripheries" := "2"]
  PushSymbol s  -> [ "shape" := "rect", "label" := sym s, "color" := "green", "style" := "dashed"]
  PopSymbol s   -> [ "shape" := "diamond", "label" := sym s, "color" := "green", "style" := "dashed"]
  PushScope     -> [ "shape" := "rect", "label" := ("PUSH " <> show idx)]
  Scope s       -> [ "shape" := "circle", "label" := sym s, "style" := "filled"]
  ExportedScope s -> [ "shape" := "circle", "label" := sym s]
  JumpToScope s  -> [ "shape" := "circle", "label" := sym s]
  IgnoreScope   -> [ "shape" := "rect", "label" := "IGNORE", "color" := "purple"]
  Root _         -> [ "shape" := "circle", "style" := "filled", "label" := "root", "fillcolor" := "black", "fontcolor" := "white"]



nodeStyle :: Dot.Style (Tagged Node) String
nodeStyle = Dot.Style
  { Dot.graphName = "stack_graph"
  , Dot.preamble = []
  , Dot.graphAttributes = []
  , Dot.defaultVertexAttributes = []
  , Dot.defaultEdgeAttributes = []
  , Dot.vertexName = nodeToDotName
  , Dot.vertexAttributes = nodeAttributes
  , Dot.edgeAttributes = mempty
  }

toGraphViz :: Graph (Tagged Node) -> String
toGraphViz = Dot.export nodeStyle

openGraphViz :: Graph Node -> IO ()
openGraphViz (tagGraphUniquely -> g) = do
  -- Not using streaming-process's temporary file support, because we don't want
  -- to clean this up immediately after we're done using it.
  (pngPath, pngH) <- openTempFile "/tmp" "stack-graph.svg"
  let dotProc = Process.proc "dot" ["-Tsvg"]
  putStrLn (toGraphViz g)
  Streaming.Process.withStreamingProcess dotProc (ByteStream.string (toGraphViz g))
    (void . ByteStream.hPut pngH . hoist ByteStream.putStrLn)
  hFlush pngH
  void $ system ("open " <> pngPath)
