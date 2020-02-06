module Tags.Tagging.Precise
( Tags
, ToTags(..)
, yield
, runTagging
, firstLine
) where

import Control.Carrier.Reader
import Control.Carrier.Writer.Strict
import Data.Functor.Identity
import Data.Monoid (Endo (..))
import Data.Text as Text (Text, take, takeWhile, stripEnd)
import Prelude hiding (span)
import Source.Loc (Loc (..))
import Source.Source as Source
import Source.Range (Range)
import Source.Span
import Tags.Tag

type Tags = Endo [Tag]

class ToTags t where
  tags :: Source -> t Loc -> [Tag]


yield :: Has (Writer Tags) sig m => Tag -> m ()
yield = tell . Endo . (:) . modSpan toOneIndexed where
  modSpan f t@Tag{ loc = l } = t { loc = l { span = f (span l) } }
  toOneIndexed (Span (Pos l1 c1) (Pos l2 c2)) = Span (Pos (l1 + 1) (c1 + 1)) (Pos (l2 + 1) (c2 + 1))

runTagging :: Source -> ReaderC Source (WriterC Tags Identity) () -> [Tag]
runTagging source
  = ($ [])
  . appEndo
  . run
  . execWriter
  . runReader source

-- | Slices a range out of 'Source' and gives back the first line of source up to 180 characters.
firstLine :: Source -> Range -> Text
firstLine src = Text.stripEnd . Text.take 180 . Text.takeWhile (/= '\n') . Source.toText . slice src
