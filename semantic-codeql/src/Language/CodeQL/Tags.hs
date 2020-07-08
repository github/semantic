{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.CodeQL.Tags
  ( ToTags (..),
  )
where

import AST.Element
import qualified AST.Parse as Parse
import AST.Token
import AST.Traversable1
import Control.Effect.Reader
import Control.Effect.Writer
import Control.Effect.State
import Data.Foldable (for_)
import qualified Language.CodeQL.AST as CodeQL
import Proto.Semantic as P
import Source.Loc
import Source.Source as Source
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags ::
    ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
      Has (Writer Tags.Tags) sig m
    ) =>
    t Loc ->
    m ()
  default tags ::
    ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
      Has (Writer Tags.Tags) sig m,
      Traversable1 ToTags t
    ) =>
    t Loc ->
    m ()
  tags = gtags

instance ToTags (Token sym n) where tags _ = pure ()

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

gtags ::
  ( Has (Reader Source) sig m,
      Has (State Tags.LineIndices) sig m,
    Has (Writer Tags.Tags) sig m,
    Traversable1 ToTags t
  ) =>
  t Loc ->
  m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

instance ToTags CodeQL.Module where
  tags
    t@CodeQL.Module
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.ModuleName {extraChildren = Parse.Success (CodeQL.SimpleId {text, ann})})
      } = Tags.yield text P.MODULE P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.ClasslessPredicate where
  tags
    t@CodeQL.ClasslessPredicate
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.PredicateName {text, ann})
      } = Tags.yield text P.FUNCTION P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.AritylessPredicateExpr where
  tags
    t@CodeQL.AritylessPredicateExpr
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.LiteralId {text, ann})
      } = Tags.yield text P.CALL P.REFERENCE ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.Dataclass where
  tags
    t@CodeQL.Dataclass
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.ClassName {text, ann})
      } = Tags.yield text P.CLASS P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.MemberPredicate where
  tags
    t@CodeQL.MemberPredicate
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.PredicateName {text, ann})
      } = Tags.yield text P.METHOD P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.Datatype where
  tags
    t@CodeQL.Datatype
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.ClassName {text, ann})
      } = Tags.yield text P.CLASS P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.DatatypeBranch where
  tags
    t@CodeQL.DatatypeBranch
      { ann = Loc {byteRange},
        name = Parse.Success (CodeQL.ClassName {text, ann})
      } = Tags.yield text P.CLASS P.DEFINITION ann byteRange >> gtags t
  tags _ = pure ()

instance ToTags CodeQL.ClasslessPredicateCall where
  tags
    CodeQL.ClasslessPredicateCall
      { extraChildren
      } = for_ extraChildren $ \x -> case x of
      EPrj t@CodeQL.AritylessPredicateExpr {} -> tags t
      _ -> pure ()

instance ToTags CodeQL.QualifiedRhs where
  tags
    t@CodeQL.QualifiedRhs
      { ann = Loc {byteRange},
        name = expr
      } = case expr of
      Just (EPrj CodeQL.PredicateName {text, ann}) -> Tags.yield text P.CALL P.REFERENCE ann byteRange >> gtags t
      _ -> gtags t

instance ToTags CodeQL.TypeExpr where
  tags
    t@CodeQL.TypeExpr
      { ann = Loc {byteRange},
        name = expr
      } = case expr of
      Just (EPrj CodeQL.ClassName {text, ann}) -> Tags.yield text P.TYPE P.REFERENCE ann byteRange >> gtags t
      _ -> gtags t

instance ToTags CodeQL.AddExpr
instance ToTags CodeQL.Addop
instance ToTags CodeQL.AggId
instance ToTags CodeQL.Aggregate
instance ToTags CodeQL.AnnotArg
instance ToTags CodeQL.Annotation
instance ToTags CodeQL.AnnotName
instance ToTags CodeQL.Any
instance ToTags CodeQL.As
instance ToTags CodeQL.Asc
instance ToTags CodeQL.AsExpr
instance ToTags CodeQL.AsExprs
instance ToTags CodeQL.Avg
instance ToTags CodeQL.Body
instance ToTags CodeQL.Bool
instance ToTags CodeQL.Boolean
instance ToTags CodeQL.Charpred
instance ToTags CodeQL.ClassMember
instance ToTags CodeQL.ClassName
instance ToTags CodeQL.Closure
instance ToTags CodeQL.Compop
instance ToTags CodeQL.CompTerm
instance ToTags CodeQL.Concat
instance ToTags CodeQL.Conjunction
instance ToTags CodeQL.Count
instance ToTags CodeQL.Class
instance ToTags CodeQL.DatatypeBranches
instance ToTags CodeQL.Date
instance ToTags CodeQL.Dbtype
instance ToTags CodeQL.Desc
instance ToTags CodeQL.Direction
instance ToTags CodeQL.Disjunction
instance ToTags CodeQL.Empty
instance ToTags CodeQL.Eq
instance ToTags CodeQL.Exists
instance ToTags CodeQL.ExprAggregateBody
instance ToTags CodeQL.Extends
instance ToTags CodeQL.False
instance ToTags CodeQL.Field
instance ToTags CodeQL.Float
instance ToTags CodeQL.Forall
instance ToTags CodeQL.Forex
instance ToTags CodeQL.FullAggregateBody
instance ToTags CodeQL.Ge
instance ToTags CodeQL.Gt
instance ToTags CodeQL.HigherOrderTerm
instance ToTags CodeQL.IfTerm
instance ToTags CodeQL.Implication
instance ToTags CodeQL.Import
instance ToTags CodeQL.ImportModuleExpr
instance ToTags CodeQL.Imprt
instance ToTags CodeQL.In
instance ToTags CodeQL.InExpr
instance ToTags CodeQL.InstanceOf
instance ToTags CodeQL.Instanceof
instance ToTags CodeQL.Integer
instance ToTags CodeQL.Le
instance ToTags CodeQL.Literal
instance ToTags CodeQL.LiteralId
instance ToTags CodeQL.Lt
instance ToTags CodeQL.Max
instance ToTags CodeQL.Mod
instance ToTags CodeQL.ModuleAliasBody
instance ToTags CodeQL.ModuleExpr
instance ToTags CodeQL.ModuleMember
instance ToTags CodeQL.ModuleName
instance ToTags CodeQL.Min
instance ToTags CodeQL.Minus
instance ToTags CodeQL.MulExpr
instance ToTags CodeQL.Mulop
instance ToTags CodeQL.Ne
instance ToTags CodeQL.Negation
instance ToTags CodeQL.Newtype
instance ToTags CodeQL.None
instance ToTags CodeQL.Not
instance ToTags CodeQL.OrderBy
instance ToTags CodeQL.OrderBys
instance ToTags CodeQL.ParExpr
instance ToTags CodeQL.Plus
instance ToTags CodeQL.Predicate
instance ToTags CodeQL.PredicateAliasBody
instance ToTags CodeQL.PredicateExpr
instance ToTags CodeQL.PredicateName
instance ToTags CodeQL.PrefixCast
instance ToTags CodeQL.Ql
instance ToTags CodeQL.Qldoc
instance ToTags CodeQL.QualifiedExpr
instance ToTags CodeQL.QualModuleExpr
instance ToTags CodeQL.Quantified
instance ToTags CodeQL.Quantifier
instance ToTags CodeQL.Range
instance ToTags CodeQL.Rank
instance ToTags CodeQL.Result
instance ToTags CodeQL.ReturnType
instance ToTags CodeQL.Select
instance ToTags CodeQL.SimpleId
instance ToTags CodeQL.Slash
instance ToTags CodeQL.SpecialCall
instance ToTags CodeQL.SpecialId
instance ToTags CodeQL.Star
instance ToTags CodeQL.Strictconcat
instance ToTags CodeQL.Strictcount
instance ToTags CodeQL.Strictsum
instance ToTags CodeQL.String
instance ToTags CodeQL.Sum
instance ToTags CodeQL.Super
instance ToTags CodeQL.SuperRef
instance ToTags CodeQL.This
instance ToTags CodeQL.True
instance ToTags CodeQL.TypeAliasBody
instance ToTags CodeQL.TypeLiteral
instance ToTags CodeQL.UnaryExpr
instance ToTags CodeQL.Underscore
instance ToTags CodeQL.Unop
instance ToTags CodeQL.VarDecl
instance ToTags CodeQL.Variable
instance ToTags CodeQL.VarName
