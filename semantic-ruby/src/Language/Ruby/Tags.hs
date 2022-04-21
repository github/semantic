{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}


module Language.Ruby.Tags
  ( ToTags (..),
  )
where

import           AST.Element
import qualified AST.Parse as Parse
import           AST.Token
import           AST.Traversable1
import qualified AST.Unmarshal as TS
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Writer
import           Control.Monad
import           Data.Foldable
import           Data.Text (Text)
import qualified Language.Ruby.AST as Rb
import           Proto.Semantic as P
import           Source.Loc
import           Source.Range as Range
import           Source.Source as Source
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags ::
    ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
      Has (Writer Tags.Tags) sig m,
      Has (State [Text]) sig m
    ) =>
    t Loc ->
    m ()
  default tags ::
    ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
      Has (Writer Tags.Tags) sig m,
      Has (State [Text]) sig m,
      Traversable1 ToTags t
    ) =>
    t Loc ->
    m ()
  tags = gtags

instance ToTags (Token sym n) where tags _ = pure ()

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

-- These are all valid, but point to methods in Kernel and other parts of the
-- Ruby stdlib. A la carte displays some of these, but not others and since we
-- have nothing to link to yet (can't jump-to-def), we hide them from the
-- current tags output.
nameBlacklist :: [Text]
nameBlacklist =
  [ "alias",
    "load",
    "require_relative",
    "require",
    "super",
    "undef",
    "__FILE__",
    "__LINE__",
    "lambda"
  ]

yieldTag :: (Has (Reader Source) sig m, Has (State Tags.LineIndices) sig m, Has (Writer Tags.Tags) sig m) => Text -> P.SyntaxType -> P.NodeType -> Loc -> Range -> m ()
yieldTag name P.CALL _ _ _ | name `elem` nameBlacklist = pure ()
yieldTag name kind ty loc srcLineRange = Tags.yield name kind ty loc srcLineRange

instance ToTags Rb.Class where
  tags
    t@Rb.Class
      { ann = Loc {byteRange = Range {start}},
        name = Parse.Success expr,
        extraChildren
      } = enterScope True $ case expr of
      Prj Rb.Constant {text, ann}                                    -> yield text ann
      Prj Rb.ScopeResolution {name = EPrj Rb.Constant {text, ann}}   -> yield text ann
      Prj Rb.ScopeResolution {name = EPrj Rb.Identifier {text, ann}} -> yield text ann
      _                                                              -> gtags t
      where
        range' = case extraChildren of
          EPrj Rb.Superclass {ann = Loc {byteRange = Range {end}}} : _ -> Range start end
          _                                                            -> Range start (getEnd expr)
        getEnd = Range.end . byteRange . TS.gann
        yield name loc = yieldTag name P.CLASS P.DEFINITION loc range' >> gtags t
  tags _ = pure ()

instance ToTags Rb.SingletonClass where
  tags
    t@Rb.SingletonClass
      { ann = Loc {byteRange = range@Range {start}},
        value = Parse.Success (Rb.Arg expr),
        extraChildren
      } = enterScope True $ case expr of
      Prj (Rb.Primary (Prj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Constant {text, ann}))))))                -> yield text ann
      Prj (Rb.Primary (Prj (Rb.Lhs (Prj Rb.ScopeResolution {name = EPrj Rb.Constant {text, ann}}))))   -> yield text ann
      Prj (Rb.Primary (Prj (Rb.Lhs (Prj Rb.ScopeResolution {name = EPrj Rb.Identifier {text, ann}})))) -> yield text ann
      _                                                                                                -> gtags t
      where
        range' = case extraChildren of
          Parse.Success x : _ -> Range start (getStart x)
          _                   -> range
        getStart = Range.start . byteRange . TS.gann
        yield name loc = yieldTag name P.CLASS P.DEFINITION loc range' >> gtags t
  tags _ = pure ()

instance ToTags Rb.Module where
  tags
    t@Rb.Module
      { ann = Loc {byteRange = Range {start}},
        name = Parse.Success expr,
        extraChildren
      } = enterScope True $ case expr of
      Prj Rb.Constant {text, ann}                                    -> yield text ann
      Prj Rb.ScopeResolution {name = EPrj Rb.Constant {text, ann}}   -> yield text ann
      Prj Rb.ScopeResolution {name = EPrj Rb.Identifier {text, ann}} -> yield text ann
      _                                                              -> gtags t
      where
        range' = case extraChildren of
          Parse.Success x : _ -> Range start (getStart x)
          _                   -> Range start (getEnd expr)
        getEnd = Range.end . byteRange . TS.gann
        getStart = Range.start . byteRange . TS.gann
        yield name loc = yieldTag name P.MODULE P.DEFINITION loc range' >> gtags t
  tags _ = pure ()

yieldMethodNameTag ::
  ( Has (State [Text]) sig m,
    Has (Reader Source) sig m,
    Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  Range ->
  Rb.MethodName Loc ->
  m ()
yieldMethodNameTag t range (Rb.MethodName expr) = enterScope True $ case expr of
  Prj Rb.Identifier {text, ann}                                             -> yield text ann
  Prj Rb.Constant {text, ann}                                               -> yield text ann
  -- Prj Rb.ClassVariable { text = name } -> yield name
  Prj Rb.Operator {text, ann}                                               -> yield text ann
  -- Prj Rb.GlobalVariable { text = name } -> yield name
  -- Prj Rb.InstanceVariable { text = name } -> yield name
  Prj Rb.Setter {extraChildren = Parse.Success (Rb.Identifier {text, ann})} -> yield (text <> "=") ann-- NB: Matches existing tags output, TODO: Remove this.
        -- TODO: Should we report symbol method names as tags?
        -- Prj Rb.Symbol { extraChildren = [Prj Rb.EscapeSequence { text = name }] } -> yield name
  _                                                                         -> gtags t
  where
    yield name loc = yieldTag name P.METHOD P.DEFINITION loc range >> gtags t

enterScope :: (Has (State [Text]) sig m) => Bool -> m () -> m ()
enterScope createNew m = do
  locals <- get @[Text]
  when createNew $ put @[Text] [] -- NB: Matches existing behavior in assignment, not necessarily correct
  m
  put locals

instance ToTags Rb.Method where
  tags
    t@Rb.Method
      { ann = Loc {byteRange = Range {start}},
        name = Parse.Success n,
        parameters
      } = yieldMethodNameTag t range' n
      where
        range' = case parameters of
          Just (Parse.Success (Rb.MethodParameters {ann = Loc {byteRange = Range {end}}})) -> Range start end
          _                                                                                -> Range start (getEnd n)
        getEnd = Range.end . byteRange . TS.gann
  tags _ = pure ()

instance ToTags Rb.SingletonMethod where
  tags
    t@Rb.SingletonMethod
      { ann = Loc {byteRange = Range {start}},
        name = Parse.Success n,
        parameters
      } = yieldMethodNameTag t range' n
      where
        range' = case parameters of
          Just (Parse.Success (Rb.MethodParameters {ann = Loc {byteRange = Range {end}}})) -> Range start end
          _                                                                                -> Range start (getEnd n)
        getEnd = Range.end . byteRange . TS.gann
  tags _ = pure ()

instance ToTags Rb.Block where
  tags = enterScope False . gtags

instance ToTags Rb.DoBlock where
  tags = enterScope False . gtags

instance ToTags Rb.Lambda where
  tags Rb.Lambda {body = Parse.Success b, parameters} = enterScope False $ do
    case parameters of
      Just (Parse.Success p) -> tags p
      _                      -> pure ()
    tags b
  tags _ = pure ()

instance ToTags Rb.If where
  tags Rb.If {condition = Parse.Success cond, consequence, alternative} = do
    tags cond
    case consequence of
      Just (Parse.Success cons) -> tags cons
      _                         -> pure ()
    case alternative of
      Just (Parse.Success alt) -> tags alt
      _                        -> pure ()
  tags _ = pure ()

instance ToTags Rb.Elsif where
  tags Rb.Elsif {condition = Parse.Success cond, consequence, alternative} = do
    tags cond
    case consequence of
      Just (Parse.Success cons) -> tags cons
      _                         -> pure ()
    case alternative of
      Just (Parse.Success alt) -> tags alt
      _                        -> pure ()
  tags _ = pure ()

instance ToTags Rb.Unless where
  tags Rb.Unless {condition = Parse.Success cond, consequence, alternative} = do
    tags cond
    case consequence of
      Just (Parse.Success cons) -> tags cons
      _                         -> pure ()
    case alternative of
      Just (Parse.Success alt) -> tags alt
      _                        -> pure ()
  tags _ = pure ()

instance ToTags Rb.While where
  tags Rb.While {condition = Parse.Success cond, body = Parse.Success b} = tags cond >> tags b
  tags _                                                                 = pure ()

instance ToTags Rb.Until where
  tags Rb.Until {condition = Parse.Success cond, body = Parse.Success b} = tags cond >> tags b
  tags _                                                                 = pure ()

instance ToTags Rb.Regex where
  tags Rb.Regex {} = pure ()

instance ToTags Rb.Subshell where
  tags Rb.Subshell {} = pure ()

-- TODO: Line of source produced here could be better.
instance ToTags Rb.Lhs where
  tags t@(Rb.Lhs expr) = case expr of
    -- NOTE: Calls do not look for locals
    Prj Rb.Call {ann = Loc {byteRange}, method} -> case method of
      EPrj Rb.Identifier {text, ann} -> yieldCall text ann byteRange
      EPrj Rb.Constant {text, ann}   -> yieldCall text ann byteRange
      EPrj Rb.Operator {text, ann}   -> yieldCall text ann byteRange
      _                              -> gtags t
    -- These do check for locals before yielding a call tag
    Prj (Rb.Variable (Prj Rb.Identifier {ann = loc@Loc {byteRange}, text})) -> yield text P.CALL loc byteRange
    Prj Rb.ScopeResolution {ann = loc@Loc {byteRange}, name = EPrj Rb.Identifier {text}} -> yield text P.CALL loc byteRange
    Prj (Rb.Variable (Prj Rb.Constant { ann = loc@Loc { byteRange }, text })) -> yield text P.CALL loc byteRange -- TODO: Should yield Constant
    Prj Rb.ScopeResolution { ann = loc@Loc { byteRange }, name = EPrj Rb.Constant { text } } -> yield text P.CALL loc byteRange -- TODO: Should yield Constant
    _ -> gtags t
    where
      yieldCall name loc range = yieldTag name P.CALL P.REFERENCE loc range >> gtags t
      yield name kind loc range = do
        locals <- get @[Text]
        unless (name `elem` locals) $ yieldTag name kind P.REFERENCE loc range
        gtags t

-- TODO: Line of source produced here could be better.
instance ToTags Rb.MethodCall where
  tags
    t@Rb.MethodCall
      { ann = Loc {byteRange = byteRange@Range {}},
        method = expr
      } = case expr of
      EPrj (Rb.Variable (Prj Rb.Identifier {text, ann})) -> yield text P.CALL ann
      EPrj (Rb.Variable (Prj Rb.Constant {text, ann})) -> yield text P.CALL ann -- TODO: Should yield Constant
      EPrj Rb.ScopeResolution {name = EPrj Rb.Identifier {text, ann}} -> yield text P.CALL ann
      EPrj Rb.ScopeResolution {name = EPrj Rb.Constant {text, ann}} -> yield text P.CALL ann -- TODO: Should yield Constant
      EPrj Rb.Call {method} -> case method of
        EPrj Rb.Identifier {text, ann} -> yield text P.CALL ann
        EPrj Rb.Constant {text, ann}   -> yield text P.CALL ann
        EPrj Rb.Operator {text, ann}   -> yield text P.CALL ann
        _                              -> gtags t
      _ -> gtags t
      where
        yield name kind loc = yieldTag name kind P.REFERENCE loc byteRange >> gtags t

instance ToTags Rb.Alias where
  tags
    t@Rb.Alias
      { alias = Parse.Success (Rb.MethodName aliasExpr),
        name = Parse.Success (Rb.MethodName nameExpr),
        ann = Loc {byteRange}
      } = do
      case aliasExpr of
        Prj Rb.Identifier {ann, text} -> yieldTag text P.FUNCTION P.DEFINITION ann byteRange
        _                             -> tags aliasExpr
      case nameExpr of
        Prj Rb.Identifier {ann, text} -> yieldTag text P.CALL P.REFERENCE ann byteRange
        _                             -> tags nameExpr
      gtags t
  tags _ = pure ()

instance ToTags Rb.Undef where
  tags
    t@Rb.Undef
      { extraChildren,
        ann = Loc {byteRange}
      } = do
      for_ extraChildren $
        \case
          Parse.Success (Rb.MethodName expr) -> do
            case expr of
              Prj Rb.Identifier {ann, text} -> yieldTag text P.CALL P.REFERENCE ann byteRange
              _                             -> tags expr
          Parse.Fail _ -> pure ()
      gtags t


introduceLocals ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Has (State [Text]) sig m
  ) =>
  [ Parse.Err
      ( (:+:)
          Rb.BlockParameter
          ( Rb.DestructuredParameter
              :+: ( Rb.HashSplatParameter
                      :+: ( Rb.Identifier
                              :+: ( Rb.KeywordParameter
                                      :+: (Rb.OptionalParameter :+: Rb.SplatParameter)
                                  )
                          )
                  )
          )
          Loc
      )
  ] ->
  m ()
introduceLocals params = for_ params $ \param -> case param of
  EPrj Rb.BlockParameter {name = Parse.Success (Rb.Identifier {text = lvar})} -> modify (lvar :)
  EPrj Rb.DestructuredParameter {extraChildren} -> introduceLocals extraChildren
  EPrj Rb.HashSplatParameter {name = Just (Parse.Success (Rb.Identifier {text = lvar}))} -> modify (lvar :)
  EPrj Rb.Identifier {text = lvar} -> modify (lvar :)
  EPrj Rb.KeywordParameter {name = Parse.Success (Rb.Identifier {text = lvar})} -> modify (lvar :)
  EPrj Rb.OptionalParameter {name = Parse.Success (Rb.Identifier {text = lvar})} -> modify (lvar :)
  EPrj Rb.SplatParameter {name = Just (Parse.Success (Rb.Identifier {text = lvar}))} -> modify (lvar :)
  _ -> pure ()

instance ToTags Rb.MethodParameters where
  tags t@Rb.MethodParameters {extraChildren} = introduceLocals extraChildren >> gtags t

instance ToTags Rb.LambdaParameters where
  tags t@Rb.LambdaParameters {extraChildren} = introduceLocals extraChildren >> gtags t

instance ToTags Rb.BlockParameters where
  tags t@Rb.BlockParameters {extraChildren} = introduceLocals extraChildren >> gtags t

instance ToTags Rb.Assignment where
  tags t@Rb.Assignment {left} = do
    case left of
      EPrj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier {text})))) -> modify (text :)
      EPrj Rb.LeftAssignmentList {extraChildren}                   -> introduceLhsLocals extraChildren
      _                                                            -> pure ()
    gtags t
    where
      introduceLhsLocals xs = for_ xs $ \x -> case x of
        EPrj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier {text})))) -> modify (text :)
        EPrj Rb.DestructuredLeftAssignment {extraChildren} -> introduceLhsLocals extraChildren
        EPrj Rb.RestAssignment {extraChildren = Just (Parse.Success (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier {text})))))} -> modify (text :)
        _ -> pure ()

instance ToTags Rb.OperatorAssignment where
  tags t@Rb.OperatorAssignment {left} = do
    case left of
      EPrj (Rb.Lhs (Prj (Rb.Variable (Prj Rb.Identifier {text})))) -> modify (text :)
      _                                                            -> pure ()
    gtags t

gtags ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Has (State [Text]) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  m ()
gtags = traverse1_ @ToTags (const (pure ())) tags


-- instance ToTags Rb.Alias
instance ToTags Rb.Arg
instance ToTags Rb.ArgumentList
instance ToTags Rb.Array
-- instance ToTags Rb.Assignment
instance ToTags Rb.BareString
instance ToTags Rb.BareSymbol
instance ToTags Rb.Begin
instance ToTags Rb.BeginBlock
instance ToTags Rb.Binary
-- instance ToTags Rb.Block
instance ToTags Rb.BlockArgument
instance ToTags Rb.BlockParameter
-- instance ToTags Rb.BlockParameters
instance ToTags Rb.Break
instance ToTags Rb.Call
instance ToTags Rb.Case
instance ToTags Rb.ChainedString
instance ToTags Rb.Character
-- instance ToTags Rb.Class
instance ToTags Rb.ClassVariable
instance ToTags Rb.Complex
instance ToTags Rb.Conditional
instance ToTags Rb.Constant
instance ToTags Rb.DestructuredLeftAssignment
instance ToTags Rb.DestructuredParameter
instance ToTags Rb.Do
-- instance ToTags Rb.DoBlock
instance ToTags Rb.ElementReference
instance ToTags Rb.Else
-- instance ToTags Rb.Elsif
instance ToTags Rb.EmptyStatement
instance ToTags Rb.EndBlock
instance ToTags Rb.Ensure
instance ToTags Rb.EscapeSequence
instance ToTags Rb.ExceptionVariable
instance ToTags Rb.Exceptions
instance ToTags Rb.False
instance ToTags Rb.Float
instance ToTags Rb.For
instance ToTags Rb.GlobalVariable
instance ToTags Rb.Hash
instance ToTags Rb.HashSplatArgument
instance ToTags Rb.HashSplatParameter
instance ToTags Rb.HeredocBeginning
instance ToTags Rb.HeredocEnd
instance ToTags Rb.Identifier
-- instance ToTags Rb.If
instance ToTags Rb.IfModifier
instance ToTags Rb.In
instance ToTags Rb.InstanceVariable
instance ToTags Rb.Integer
instance ToTags Rb.Interpolation
instance ToTags Rb.KeywordParameter
-- instance ToTags Rb.Lambda
-- instance ToTags Rb.LambdaParameters
instance ToTags Rb.LeftAssignmentList
-- instance ToTags Rb.Lhs
-- instance ToTags Rb.Method
-- instance ToTags Rb.MethodCall
instance ToTags Rb.MethodName
-- instance ToTags Rb.MethodParameters
-- instance ToTags Rb.Module
instance ToTags Rb.Next
instance ToTags Rb.Nil
instance ToTags Rb.Operator
-- instance ToTags Rb.OperatorAssignment
instance ToTags Rb.OptionalParameter
instance ToTags Rb.Pair
instance ToTags Rb.ParenthesizedStatements
instance ToTags Rb.Pattern
instance ToTags Rb.Primary
instance ToTags Rb.Program
instance ToTags Rb.Range
instance ToTags Rb.Rational
instance ToTags Rb.Redo
-- instance ToTags Rb.Regex
instance ToTags Rb.Rescue
instance ToTags Rb.RescueModifier
instance ToTags Rb.RestAssignment
instance ToTags Rb.Retry
instance ToTags Rb.Return
instance ToTags Rb.RightAssignmentList
instance ToTags Rb.ScopeResolution
instance ToTags Rb.Self
instance ToTags Rb.Setter
-- instance ToTags Rb.SingletonClass
-- instance ToTags Rb.SingletonMethod
instance ToTags Rb.SplatArgument
instance ToTags Rb.SplatParameter
instance ToTags Rb.Statement
instance ToTags Rb.String
instance ToTags Rb.StringArray
-- instance ToTags Rb.Subshell
instance ToTags Rb.Super
instance ToTags Rb.Superclass
instance ToTags Rb.Symbol
instance ToTags Rb.SymbolArray
instance ToTags Rb.Then
instance ToTags Rb.True
instance ToTags Rb.Unary
-- instance ToTags Rb.Undef
instance ToTags Rb.Uninterpreted
-- instance ToTags Rb.Unless
instance ToTags Rb.UnlessModifier
-- instance ToTags Rb.Until
instance ToTags Rb.UntilModifier
instance ToTags Rb.Variable
instance ToTags Rb.When
-- instance ToTags Rb.While
instance ToTags Rb.WhileModifier
instance ToTags Rb.Yield
