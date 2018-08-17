module Language.JSON.Translate where

import Control.Rule
import Data.Language
import Data.Reprinting.Token
import Data.Reprinting.Splice
import Reprinting.Translate
import Data.Sequence hiding (fromFunction)
import           Data.Machine

data JSONTypeSetting = JSONTypeSetting { jsonPrettyPrint :: Bool }
  deriving (Eq, Show)

prettyJSON :: JSONTypeSetting
prettyJSON = JSONTypeSetting True

translatingJSON :: Rule eff Splice (Seq Splice)
translatingJSON = fromFunction "translatingJSON" step where
  step (Insert el@(Truth True) c _) = splice el c "True"
  step x = pure x

beautifyingJSON :: JSONTypeSetting -> Rule eff Splice (Seq Splice)
beautifyingJSON _ = fromFunction "beautifyingJSON" step where
  step s@(Insert Open (Just List) _) = s <| directive (HardWrap 2 Space)

minimizingJSON :: Rule eff Token (Seq Splice)
minimizingJSON = undefined

-- instance Translation 'JSON JSONTypeSetting where
--   translation _ JSONTypeSetting{..} content context = undefined -- case (content, context) of
    -- (Fragment f, _) -> Right $ splice f
    --
    -- (Truth True, _)  -> Right $ splice "true"
    -- (Truth False, _) -> Right $ splice "false"
    -- (Nullity, _)     -> Right $ splice "null"
    --
    -- (Open, List:_)        -> Right $ splice "["
    -- (Open, Associative:_) -> Right $ splice "{" <>
    --   if jsonPrettyPrint then directive (HardWrap 2 Space) else mempty
    --
    -- (Close, List:_)        -> Right $ splice "]"
    -- (Close, Associative:_) ->
    --   let prefix = if jsonPrettyPrint then directive (HardWrap 0 Space) else mempty
    --   in Right $ prefix <> splice "}"
    --
    -- (Separator, List:_)        -> Right $ splice ","
    -- (Separator, Associative:_) -> Right $ splice ","
    -- (Separator, Pair:_)        -> Right $ splice ":"
    --
    -- _ -> Left "JSON translate failed, unknown context"
