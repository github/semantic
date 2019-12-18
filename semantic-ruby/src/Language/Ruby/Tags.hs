{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DisambiguateRecordFields, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, NamedFieldPuns, OverloadedStrings, PartialTypeSignatures, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module Language.Ruby.Tags
( ToTags(..)
) where

import           AST.Element
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Writer
import           Control.Monad
import           Data.Monoid (Ap (..))
import           Data.Foldable
import           Data.Text as Text
import           GHC.Generics
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags
import qualified TreeSitter.Ruby.AST as Rb

class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       , Has (State [Text]) sig m
       )
    => t Loc
    -> m ()

instance (ToTagsBy strategy t, strategy ~ ToTagsInstance t) => ToTags t where
  tags = tags' @strategy


class ToTagsBy (strategy :: Strategy) t where
  tags'
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       , Has (State [Text]) sig m
       )
    => t Loc
    -> m ()


data Strategy = Generic | Custom

type family ToTagsInstance t :: Strategy where
  ToTagsInstance (_ :+: _)             = 'Custom
  ToTagsInstance Rb.Class              = 'Custom
  ToTagsInstance Rb.SingletonClass     = 'Custom
  ToTagsInstance Rb.Module             = 'Custom

  ToTagsInstance Rb.Method             = 'Custom
  ToTagsInstance Rb.SingletonMethod    = 'Custom

  ToTagsInstance Rb.Call               = 'Custom
  ToTagsInstance Rb.Lhs                = 'Custom
  ToTagsInstance Rb.MethodCall         = 'Custom
  ToTagsInstance Rb.Alias              = 'Custom
  ToTagsInstance Rb.Undef              = 'Custom

  -- Along with class, module, and method definitions, these introduce new lexical scopes for locals
  ToTagsInstance Rb.Block              = 'Custom
  ToTagsInstance Rb.DoBlock            = 'Custom
  ToTagsInstance Rb.Lambda             = 'Custom

  -- Parameters and assignment introduce locals
  ToTagsInstance Rb.MethodParameters   = 'Custom
  ToTagsInstance Rb.LambdaParameters   = 'Custom
  ToTagsInstance Rb.BlockParameters    = 'Custom
  ToTagsInstance Rb.Assignment         = 'Custom

  ToTagsInstance _                     = 'Generic

instance (ToTags l, ToTags r) => ToTagsBy 'Custom (l :+: r) where
  tags' (L1 l) = tags l
  tags' (R1 r) = tags r

-- These are all valid, but point to methods in Kernel and other parts of the
-- Ruby stdlib. A la carte displays some of these, but not others and since we
-- have nothing to link to yet (can't jump-to-def), we hide them from the
-- current tags output.
nameBlacklist :: [Text]
nameBlacklist =
  [ "alias"
  , "load"
  , "require_relative"
  , "require"
  , "super"
  , "undef"
  , "__FILE__"
  , "lambda"
  ]

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name Call _ _ | name `elem` nameBlacklist = pure ()
yieldTag name kind loc range = do
  src <- ask @Source
  let sliced = slice src range
  Tags.yield (Tag name kind loc (Tags.firstLine sliced) Nothing)

instance ToTagsBy 'Custom Rb.Class where
  tags' t@Rb.Class
    { ann = loc@Loc { byteRange = range }
    , name = expr
    } = enterScope True $ case expr of
      Prj Rb.Constant { text } -> yield text
      Prj Rb.ScopeResolution { name = Prj Rb.Constant { text } } -> yield text
      Prj Rb.ScopeResolution { name = Prj Rb.Identifier { text } } -> yield text
      _ -> gtags t
    where
      yield name = yieldTag name Class loc range >> gtags t

instance ToTagsBy 'Custom Rb.SingletonClass where
  tags' t@Rb.SingletonClass
    { ann = loc@Loc { byteRange = range }
    , value = Rb.Arg expr
    } = enterScope True $ case expr of
      Prj (Rb.Primary (Prj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Constant { text })))))) -> yield text
      _ -> gtags t
    where
      yield name = yieldTag name Class loc range >> gtags t

instance ToTagsBy 'Custom Rb.Module where
  tags' t@Rb.Module
    { ann = loc@Loc { byteRange = range }
    , name = expr
    } = enterScope True $ case expr of
      Prj Rb.Constant { text = name } -> yield name
      Prj Rb.ScopeResolution { name = Prj Rb.Constant { text = name } } -> yield name
      Prj Rb.ScopeResolution { name = Prj Rb.Identifier { text = name } } -> yield name
      _ -> gtags t
    where
      yield name = yieldTag name Module loc range >> gtags t

yieldMethodNameTag
  :: ( Has (State [Text]) sig m
     , Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Generic1 t
     , Tags.GFoldable1 ToTags (Rep1 t)
     ) => t Loc -> Loc -> Range -> Rb.MethodName Loc -> m ()
yieldMethodNameTag t loc range (Rb.MethodName expr) = enterScope True $ case expr of
  Prj Rb.Identifier { text = name } -> yield name
  Prj Rb.Constant { text = name } -> yield name
  -- Prj Rb.ClassVariable { text = name } -> yield name
  Prj Rb.Operator { text = name } -> yield name
  -- Prj Rb.GlobalVariable { text = name } -> yield name
  -- Prj Rb.InstanceVariable { text = name } -> yield name
  Prj Rb.Setter { extraChildren = Rb.Identifier { text = name } } -> yield (name <> "=") -- NB: Matches existing tags output, TODO: Remove this.
  -- TODO: Should we report symbol method names as tags?
  -- Prj Rb.Symbol { extraChildren = [Prj Rb.EscapeSequence { text = name }] } -> yield name
  _ -> gtags t
  where
    yield name = yieldTag name Function loc range >> gtags t

enterScope :: (Has (State [Text]) sig m) => Bool -> m () -> m ()
enterScope createNew m = do
  locals <- get @[Text]
  when createNew $ put @[Text] [] -- NB: Matches existing behavior in assignment, not necessarily correct
  m
  put locals

instance ToTagsBy 'Custom Rb.Method where
  tags' t@Rb.Method
    { ann = loc@Loc { byteRange = range }
    , name = expr
    } = yieldMethodNameTag t loc range expr

instance ToTagsBy 'Custom Rb.SingletonMethod where
  tags' t@Rb.SingletonMethod
    { ann = loc@Loc { byteRange = range }
    , name = expr
    } = yieldMethodNameTag t loc range expr

instance ToTagsBy 'Custom Rb.Block where
  tags' = enterScope False . gtags

instance ToTagsBy 'Custom Rb.DoBlock where
  tags' = enterScope False . gtags

instance ToTagsBy 'Custom Rb.Lambda where
  tags' = enterScope False . gtags

instance ToTagsBy 'Custom Rb.Call where
  tags' t@Rb.Call
    { ann = loc@Loc { byteRange = range }
    -- , receiver = Rb.Primary rcv
    , method = expr
    } = do
      -- TODO: a la carte tags captures some receivers like this:
      -- case rcv of
      --   Prj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier { text = name })))) -> yield name
      --   -- Prj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Constant { text = name })))) -> yield name
      --   _ -> pure ()
      case expr of
        Prj Rb.Identifier { text = name } -> yield name Call
        Prj Rb.Constant { text = name } -> yield name Constant
        Prj Rb.Operator { text = name } -> yield name Call
        _ -> gtags t
    where
      yield name kind = yieldTag name kind loc range >> gtags t

instance ToTagsBy 'Custom Rb.Lhs where
  tags' t@(Rb.Lhs (Prj (Rb.Variable expr)))
    = case expr of
      Prj Rb.Identifier { ann = loc@Loc { byteRange = range }, text = name } -> do
        locals <- get @[Text]
        unless (name `elem` locals) $ yieldTag name Call loc range
        gtags t
      -- TODO: This would be great to track, but doesn't match current a la carte tags output
      -- Prj Rb.Constant { ann = loc@Loc { byteRange = range }, text = name } -> yieldTag name Constant loc range >> gtags t
      _ -> gtags t
  tags' t = gtags t

instance ToTagsBy 'Custom Rb.MethodCall where
  tags' t@Rb.MethodCall
    { ann = loc@Loc { byteRange = range }
    , method = expr
    } = case expr of
      Prj (Rb.Variable (Prj Rb.Identifier { text = name })) -> yield name Call
      Prj (Rb.Variable (Prj Rb.Constant { text = name })) -> yield name Constant
      -- Prj (Rb.Variable (Prj Rb.GlobalVariable { text = name })) -> yield name
      -- Prj (Rb.Variable (Prj Rb.ClassVariable { text = name })) -> yield name
      -- Prj (Rb.Variable (Prj Rb.InstanceVariable { text = name })) -> yield name
      _ -> gtags t
    where
      yield name kind = yieldTag name kind loc range >> gtags t

instance ToTagsBy 'Custom Rb.Alias where
  tags' t@Rb.Alias
    { ann = loc@Loc { byteRange = range }
    , alias = Rb.MethodName aliasExpr
    , name = Rb.MethodName nameExpr
    } = do
      case aliasExpr of
        Prj Rb.Identifier { text } -> yieldTag text Function loc range
        _ -> tags aliasExpr
      case nameExpr of
        Prj Rb.Identifier { text } -> yieldTag text Call loc range
        _ -> tags nameExpr
      gtags t

instance ToTagsBy 'Custom Rb.Undef where
  tags' t@Rb.Undef
    { ann = loc@Loc { byteRange = range }
    , extraChildren
    } = for_ extraChildren $ \(Rb.MethodName expr) -> do
      case expr of
        Prj Rb.Identifier { text } -> yieldTag text Call loc range
        _ -> tags expr
      gtags t

introduceLocals
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Has (State [Text]) sig m
     )
  => [((Rb.BlockParameter :+: Rb.DestructuredParameter :+: Rb.HashSplatParameter) :+:
      ((Rb.Identifier :+: Rb.KeywordParameter) :+: (Rb.OptionalParameter :+: Rb.SplatParameter)))
      Loc ]
  -> m ()
introduceLocals params = for_ params $ \param -> case param of
  Prj Rb.BlockParameter { name = Rb.Identifier { text = lvar } } -> modify (lvar :)
  Prj Rb.DestructuredParameter { extraChildren } -> introduceLocals extraChildren
  Prj Rb.HashSplatParameter { name = Just Rb.Identifier { text = lvar } } -> modify (lvar :)
  Prj Rb.Identifier { text = lvar } -> modify (lvar :)
  Prj Rb.KeywordParameter { name = Rb.Identifier { text = lvar }} -> modify (lvar :)
  Prj Rb.OptionalParameter { name = Rb.Identifier { text = lvar }} -> modify (lvar :)
  Prj Rb.SplatParameter { name = Just Rb.Identifier { text = lvar } } -> modify (lvar :)
  _ -> tags param

instance ToTagsBy 'Custom Rb.MethodParameters where
  tags' Rb.MethodParameters{ extraChildren } = introduceLocals extraChildren

instance ToTagsBy 'Custom Rb.LambdaParameters where
  tags' Rb.LambdaParameters{ extraChildren } = introduceLocals extraChildren

instance ToTagsBy 'Custom Rb.BlockParameters where
  tags' Rb.BlockParameters{ extraChildren } = introduceLocals extraChildren

instance ToTagsBy 'Custom Rb.Assignment where
  tags' t@Rb.Assignment{ left } = do
    case left of
      Prj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier { text })))) -> modify (text :)
      Prj Rb.LeftAssignmentList { extraChildren } -> introduceLhsLocals extraChildren
      _ -> tags left
    gtags t
    where
      introduceLhsLocals xs = for_ xs $ \x -> case x of
        Prj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier { text })))) -> modify (text :)
        Prj Rb.DestructuredLeftAssignment { extraChildren } -> introduceLhsLocals extraChildren
        Prj Rb.RestAssignment { extraChildren = Just (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier { text })))) } -> modify (text :)
        _ -> tags x

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Has (State [Text]) sig m
     , Generic1 t
     , Tags.GFoldable1 ToTags (Rep1 t)
     )
  => t Loc
  -> m ()
gtags = getAp . Tags.gfoldMap1 @ToTags (Ap . tags) . from1

instance (Generic1 t, Tags.GFoldable1 ToTags (Rep1 t)) => ToTagsBy 'Generic t where
  tags' = gtags
