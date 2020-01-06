{-# LANGUAGE AllowAmbiguousTypes      #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Language.Python.Tags
( ToTags(..)
) where

import           AST.Element
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Maybe (listToMaybe)
import           Data.Monoid (Ap(..))
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Text as Text
import           GHC.Generics
import           Source.Loc
import           Source.Range
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Python.AST as Py

class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()

instance (ToTagsBy strategy t, strategy ~ ToTagsInstance t) => ToTags t where
  tags = tags' @strategy


class ToTagsBy (strategy :: Strategy) t where
  tags'
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()


data Strategy = Generic | Custom

type family ToTagsInstance t :: Strategy where
  ToTagsInstance (_ :+: _)             = 'Custom
  ToTagsInstance Py.FunctionDefinition = 'Custom
  ToTagsInstance Py.ClassDefinition    = 'Custom
  ToTagsInstance Py.Call               = 'Custom

  -- These built-in functions all get handled as calls
  ToTagsInstance Py.AssertStatement    = 'Custom
  ToTagsInstance Py.Await              = 'Custom
  ToTagsInstance Py.DeleteStatement    = 'Custom
  ToTagsInstance Py.ExecStatement      = 'Custom
  ToTagsInstance Py.GlobalStatement    = 'Custom
  ToTagsInstance Py.NonlocalStatement  = 'Custom
  ToTagsInstance Py.PrintStatement     = 'Custom

  ToTagsInstance _                     = 'Generic


instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

keywordFunctionCall
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Generic1 t
     , Tags.GFoldable1 ToTags (Rep1 t)
     )
  => t Loc -> Loc -> Range -> Text -> m ()
keywordFunctionCall t loc range name = do
  src <- ask @Source
  let sliced = slice src range
  Tags.yield (Tag name Function loc (Tags.firstLine sliced) Nothing)
  gtags t

instance ToTagsBy 'Custom Py.AssertStatement where
  tags' t@Py.AssertStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "assert"

instance ToTagsBy 'Custom Py.Await where
  tags' t@Py.Await { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "await"

instance ToTagsBy 'Custom Py.DeleteStatement where
  tags' t@Py.DeleteStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "del"

instance ToTagsBy 'Custom Py.ExecStatement where
  tags' t@Py.ExecStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "exec"

instance ToTagsBy 'Custom Py.GlobalStatement where
  tags' t@Py.GlobalStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "global"

instance ToTagsBy 'Custom Py.NonlocalStatement where
  tags' t@Py.NonlocalStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "nonlocal"

instance ToTagsBy 'Custom Py.PrintStatement where
  tags' t@Py.PrintStatement { ann = loc@Loc { byteRange } } = keywordFunctionCall t loc byteRange "print"

instance ToTagsBy 'Custom Py.FunctionDefinition where
  tags' t@Py.FunctionDefinition
    { ann = loc@Loc { byteRange = Range { start } }
    , name = Py.Identifier { text = name }
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
          sliced = slice src (Range start end)
      Tags.yield (Tag name Function loc (Tags.firstLine sliced) docs)
      gtags t

instance ToTagsBy 'Custom Py.ClassDefinition where
  tags' t@Py.ClassDefinition
    { ann = loc@Loc { byteRange = Range { start } }
    , name = Py.Identifier { text = name }
    , body = Py.Block { ann = Loc Range { start = end } _, extraChildren }
    } = do
      src <- ask @Source
      let docs = listToMaybe extraChildren >>= docComment src
          sliced = slice src (Range start end)
      Tags.yield (Tag name Class loc (Tags.firstLine sliced) docs)
      gtags t

instance ToTagsBy 'Custom Py.Call where
  tags' t@Py.Call
    { ann = loc@Loc { byteRange = range }
    , function = Py.PrimaryExpression expr
    } = match expr
      where
        match expr = case expr of
          (Prj Py.Attribute { attribute = Py.Identifier _ name }) -> yield name
          (Prj (Py.Identifier _ name)) -> yield name
          (Prj Py.Call { function = Py.PrimaryExpression expr' }) -> match expr' -- Nested call expression like this in Python represent creating an instance of a class and calling it: e.g. AClass()()
          (Prj (Py.ParenthesizedExpression _ (Prj (Py.Expression (Prj (Py.PrimaryExpression expr')))))) -> match expr' -- Parenthesized expressions
          _ -> gtags t
        yield name = do
          src <- ask @Source
          let sliced = slice src range
          Tags.yield (Tag name Call loc (Tags.firstLine sliced) Nothing)
          gtags t


docComment :: Source -> (Py.CompoundStatement :+: Py.SimpleStatement) Loc -> Maybe Text
docComment src (R1 (Py.SimpleStatement (Prj Py.ExpressionStatement { extraChildren = L1 (Prj (Py.Expression (Prj (Py.PrimaryExpression (Prj Py.String { ann }))))) :|_ }))) = Just (toText (slice src (byteRange ann)))
docComment _ _ = Nothing


gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Generic1 t
     , Tags.GFoldable1 ToTags (Rep1 t)
     )
  => t Loc
  -> m ()
gtags = getAp . Tags.gfoldMap1 @ToTags (Ap . tags) . from1

instance (Generic1 t, Tags.GFoldable1 ToTags (Rep1 t)) => ToTagsBy 'Generic t where
  tags' = gtags
