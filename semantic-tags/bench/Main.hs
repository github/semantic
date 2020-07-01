module Main (main) where

import Tags.Tagging.Precise
import qualified Data.ByteString as B
import Gauge
import Source.Loc (Loc (..))
import Source.Range
import Source.Source as Source
import Source.Span (Pos (..), Span (..))
import Data.Char (ord)

main :: IO ()
main = do
  bytes <- fmap fromUTF8 $ B.readFile "/Users/tclem/github/github/app/models/user.rb"
  -- bytes <- fmap fromUTF8 $ B.readFile "/Users/tclem/Downloads/jquery-3.5.1.min.js"
  -- let lineIndexes = B.elemIndices (toEnum (ord '\n')) (Source.bytes bytes)
  defaultMain
    [ baselineCalculateLine bytes
    , calculateLinePRVersion bytes
    , calculateLineWithMap bytes
    ]

baselineCalculateLine :: Source -> Benchmark
baselineCalculateLine bytes = bench "baseline calculateLineAndSpans" $ do
  nf (baselineCalculateLineAndSpans bytes) (Loc (Range 73754 73757) (Span (Pos 1 73669) (Pos 1 73672)))

calculateLinePRVersion :: Source -> Benchmark
calculateLinePRVersion bytes = bench "calculateLineAndSpansPRVersion " $ do
  nf (calculateLineAndSpansPRVersion bytes) (Loc (Range 73754 73757) (Span (Pos 1 73669) (Pos 1 73672)))

calculateLineWithMap :: Source -> Benchmark
calculateLineWithMap src = bench "calculateLineAndSpansWithIndexes" $ do
  nf (calculateLineAndSpans src (LineIndices mempty)) (Loc (Range 73754 73757) (Span (Pos 1 73669) (Pos 1 73672)))
