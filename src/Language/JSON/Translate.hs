module Language.JSON.Translate where

import Control.Rule
import Data.Language
import Data.Reprinting.Token
import Data.Reprinting.Splice
import Reprinting.Translate
import Data.Sequence hiding (fromFunction)
import           Data.Machine
import           Control.Monad.Effect (Eff)

data JSONBeautyOpts = JSONBeautyOpts { jsonPrettyPrint :: Bool }
  deriving (Eq, Show)

defaultBeautyOpts :: JSONBeautyOpts
defaultBeautyOpts = JSONBeautyOpts True

translatingJSON :: ProcessT (Eff effs) Splice Splice
translatingJSON = flattened <~ auto step where
  step :: Splice -> Seq Splice
  step (Insert el@(Truth True) c _) = splice el c "True"
  step x = pure x

beautifyingJSON :: JSONBeautyOpts -> ProcessT (Eff effs) Splice Splice
beautifyingJSON _ = flattened <~ auto step where
  step :: Splice -> Seq Splice
  step s@(Insert Open  (Just Associative) _) = s <| directive (HardWrap 2 Space)
  step s@(Insert Close (Just Associative) _) = directive (HardWrap 0 Space) |> s
  step x = pure x

-- minimizingJSON :: Rule eff Token (Seq Splice)
-- minimizingJSON = undefined

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
