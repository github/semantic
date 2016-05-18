{-# LANGUAGE DataKinds, TypeFamilies, ScopedTypeVariables, FlexibleInstances, RecordWildCards #-}
module DiffSummary (DiffSummary(..), diffSummary, DiffInfo(..)) where

import Prelude hiding (fst, snd)
import Diff
import Info
import Patch
import Term
import Syntax
import Category
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Control.Monad.Trans.Free
import Control.Monad
import Data.Maybe
import Data.List
import Data.Functor.Foldable as Foldable
import Data.Functor.Both
import Data.OrderedMap
import qualified Data.Foldable as F
import Data.Text as Text (unpack, Text)

data DiffInfo = DiffInfo { name :: String, term :: Maybe String } deriving (Eq, Show)

class ToTerm a where
  toTerm :: a -> Maybe String


instance IsTerm leaf => ToTerm (Term leaf Info) where
  toTerm term = case runCofree term of
    (_ :< Leaf leaf) -> Just (termName leaf)
    (_ :< Keyed children) -> Just (unpack . mconcat $ keys children)
    (_ :< Indexed children) -> Just (termName . toCategory . head $ extract <$> children)
    (_ :< Fixed children) -> Just (termName . toCategory . head $ extract <$> children)


class IsTerm a where
  termName :: a -> String

instance IsTerm DiffInfo where
  termName = name

instance IsTerm String where
  termName = id

instance IsTerm Text where
  termName = unpack

instance IsTerm Category where
  termName category = case category of
    BinaryOperator -> "binary operator"
    DictionaryLiteral -> "dictionary literal"
    Pair -> "pair"
    FunctionCall -> "function call"
    StringLiteral -> "string literal"
    IntegerLiteral -> "integer literal"
    SymbolLiteral -> "symbol literal"
    ArrayLiteral -> "array literal"
    (Other s) -> s

data DiffSummary a = DiffSummary {
  patch :: Patch DiffInfo,
  parentAnnotations :: [DiffInfo]
} deriving (Eq, Functor)

instance Show a => Show (DiffSummary a) where
  show DiffSummary{..} = case patch of
    (Insert termInfo) -> "Added the " ++ "'" ++ fromJust (term termInfo) ++ "' " ++ termName termInfo
      ++ if null parentAnnotations then "" else " to the " ++ intercalate "#" (termName <$> parentAnnotations) ++ " context"
    (Delete termInfo) -> "Deleted the " ++ "'" ++ fromJust (term termInfo) ++ "' " ++ termName termInfo
      ++ if null parentAnnotations then "" else " in the " ++ intercalate "#" (termName <$> parentAnnotations) ++ " context"
    (Replace t1 t2) -> "Replaced the " ++ "'" ++ fromJust (term t1) ++ "' " ++ termName t1
      ++ " with the " ++ "'" ++ fromJust (term t2) ++ "' " ++ termName t2
      ++ if null parentAnnotations then "" else " in the " ++ intercalate "#" (termName <$> parentAnnotations) ++ " context"

diffSummary :: IsTerm leaf => Diff leaf Info -> [DiffSummary DiffInfo]
diffSummary = cata diffSummary' where
  diffSummary' :: IsTerm leaf => DiffF leaf Info [DiffSummary DiffInfo] -> [DiffSummary DiffInfo]
  diffSummary' (Free (_ :< Leaf _)) = [] -- Skip leaves since they don't have any changes
  diffSummary' (Free (infos :< Indexed children)) = prependSummary (DiffInfo (termName . toCategory $ snd infos) Nothing) <$> join children
  diffSummary' (Free (infos :< Fixed children)) = prependSummary (DiffInfo (termName . toCategory $ snd infos) Nothing) <$> join children
  diffSummary' (Free (infos :< Keyed children)) = prependSummary (DiffInfo (termName . toCategory $ snd infos) Nothing) <$> join (F.toList children)
  diffSummary' (Pure (Insert term)) = [DiffSummary (Insert (DiffInfo (toTermName term) (toTerm term))) []]
  diffSummary' (Pure (Delete term)) = [DiffSummary (Delete (DiffInfo (toTermName term) (toTerm term))) []]
  diffSummary' (Pure (Replace t1 t2)) = [DiffSummary (Replace (DiffInfo (toTermName t1) (toTerm t1)) (DiffInfo (toTermName t2) (toTerm t2))) []]

prependSummary :: DiffInfo -> DiffSummary DiffInfo -> DiffSummary DiffInfo
prependSummary annotation summary = summary { parentAnnotations = annotation : parentAnnotations summary }

toTermName :: IsTerm leaf => Term leaf Info -> String
toTermName term = termName $ case runCofree term of
  (info :< Leaf _) -> toCategory info
  (info :< Indexed _) -> toCategory info
  (info :< Fixed _) -> toCategory info
  (info :< Keyed _) -> toCategory info

toCategory :: Info -> Category
toCategory info = fromMaybe (Other "Unknown") (maybeFirstCategory info)
