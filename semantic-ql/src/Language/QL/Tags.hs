{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Language.QL.Tags (ToTags(..)) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Text (Text)
import           Data.List.NonEmpty(NonEmpty(..))
import qualified Language.QL.AST as QL
import           Source.Loc
import           Source.Source as Source
import           Tags.Tag
import qualified Tags.Tagging.Precise as Tags

class ToTags t where
  tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       )
    => t Loc
    -> m ()
  default tags
    :: ( Has (Reader Source) sig m
       , Has (Writer Tags.Tags) sig m
       , Traversable1 ToTags t
       )
    => t Loc
    -> m ()
  tags = gtags

instance ToTags (Token sym n) where tags _ = pure ()

instance (ToTags l, ToTags r) => ToTags (l :+: r) where
  tags (L1 l) = tags l
  tags (R1 r) = tags r

gtags
  :: ( Has (Reader Source) sig m
     , Has (Writer Tags.Tags) sig m
     , Traversable1 ToTags t
     )
  => t Loc
  -> m ()
gtags = traverse1_ @ToTags (const (pure ())) tags

yieldTag :: (Has (Reader Source) sig m, Has (Writer Tags.Tags) sig m) => Text -> Kind -> Loc -> Range -> m ()
yieldTag name kind loc range = do
  src <- ask @Source
  Tags.yield (Tag name kind loc (Tags.firstLine src range) Nothing)


instance ToTags QL.Module where
  tags t@(QL.Module loc@Loc { byteRange } term) =
    case term of
      Prj (QL.ModuleName { extraChildren = Prj (QL.SimpleId { text })}) :| _ -> yieldTag text Module loc byteRange >> gtags t
      _                                                                      -> gtags t

instance ToTags QL.ClasslessPredicate where
    tags t@(QL.ClasslessPredicate loc@Loc {byteRange } term) =
      case term of
        Prj (QL.PredicateName { text } ) :| _ -> yieldTag text Function loc byteRange >> gtags t
        _                                     -> gtags t

instance ToTags QL.AritylessPredicateExpr where
    tags t@(QL.AritylessPredicateExpr loc@Loc { byteRange } term) =
      case term of
        Prj (QL.LiteralId { text }) :| _ -> yieldTag text Call loc byteRange >> gtags t
        _                                -> gtags t

instance ToTags QL.Dataclass where
    tags t@(QL.Dataclass loc@Loc { byteRange } term) =
      case term of
        Prj (QL.ClassName { text }) :| _ -> yieldTag text Class loc byteRange >> gtags t
        _                                -> gtags t

instance ToTags QL.MemberPredicate where
    tags t@(QL.MemberPredicate loc@Loc { byteRange } term) =
      case term of
        Prj (QL.PredicateName { text }) :| _ -> yieldTag text Method loc byteRange >> gtags t
        _                                    -> gtags t

instance ToTags QL.Datatype where
    tags t@(QL.Datatype loc@Loc { byteRange } term) =
      case term of
        Prj (QL.ClassName { text }) :| _ -> yieldTag text Class loc byteRange >> gtags t
        _                                    -> gtags t

instance ToTags QL.DatatypeBranch where
    tags t@(QL.DatatypeBranch loc@Loc { byteRange } term) =
      case term of
        Prj (QL.ClassName { text }) :| _ -> yieldTag text Class loc byteRange >> gtags t
        _                                    -> gtags t

instance ToTags QL.AddExpr
instance ToTags QL.Any
instance ToTags QL.ExprAggregateBody
instance ToTags QL.ModuleName
instance ToTags QL.Strictconcat
instance ToTags QL.Addop
instance ToTags QL.Extends
instance ToTags QL.MulExpr
instance ToTags QL.Strictcount
instance ToTags QL.AggId
instance ToTags QL.As
instance ToTags QL.False
instance ToTags QL.Mulop
instance ToTags QL.Strictsum
instance ToTags QL.Aggregate
instance ToTags QL.AsExpr
instance ToTags QL.Field
instance ToTags QL.Ne
instance ToTags QL.String
instance ToTags QL.AnnotArg
instance ToTags QL.AsExprs
instance ToTags QL.Float
instance ToTags QL.Negation
instance ToTags QL.Sum
instance ToTags QL.AnnotName
instance ToTags QL.Asc
instance ToTags QL.Forall
instance ToTags QL.Newtype
instance ToTags QL.Super
instance ToTags QL.Annotation
instance ToTags QL.Avg
instance ToTags QL.Forex
instance ToTags QL.None
instance ToTags QL.SuperRef
instance ToTags QL.Body
instance ToTags QL.FullAggregateBody
instance ToTags QL.Not
instance ToTags QL.This
instance ToTags QL.Bool
instance ToTags QL.Ge
instance ToTags QL.OrderBy
instance ToTags QL.True
instance ToTags QL.Boolean
instance ToTags QL.Gt
instance ToTags QL.OrderBys
instance ToTags QL.TypeAliasBody
instance ToTags QL.Charpred
instance ToTags QL.HigherOrderTerm
instance ToTags QL.ParExpr
instance ToTags QL.TypeExpr
instance ToTags QL.IfTerm
instance ToTags QL.Plus
instance ToTags QL.TypeLiteral
instance ToTags QL.ClassMember
instance ToTags QL.Implication
instance ToTags QL.UnaryExpr
instance ToTags QL.ClassName
instance ToTags QL.Import
instance ToTags QL.PredicateAliasBody
instance ToTags QL.Underscore
instance ToTags QL.Predicate
instance ToTags QL.ImportModuleExpr
instance ToTags QL.PredicateExpr
instance ToTags QL.Unop
instance ToTags QL.Imprt
instance ToTags QL.PredicateName
instance ToTags QL.VarDecl
instance ToTags QL.Closure
instance ToTags QL.In
instance ToTags QL.PrefixCast
instance ToTags QL.VarName
instance ToTags QL.CompTerm
instance ToTags QL.InExpr
instance ToTags QL.Ql
instance ToTags QL.Variable
instance ToTags QL.Compop
instance ToTags QL.InstanceOf
instance ToTags QL.Qldoc
instance ToTags QL.Concat
instance ToTags QL.Instanceof
instance ToTags QL.QualModuleExpr
instance ToTags QL.Conjunction
instance ToTags QL.Integer
instance ToTags QL.QualifiedExpr
instance ToTags QL.QualifiedRhs
instance ToTags QL.Count
instance ToTags QL.Le
instance ToTags QL.Class
instance ToTags QL.Literal
instance ToTags QL.Quantified
instance ToTags QL.LiteralId
instance ToTags QL.Quantifier
instance ToTags QL.Lt
instance ToTags QL.Range
instance ToTags QL.DatatypeBranches
instance ToTags QL.Max
instance ToTags QL.Rank
instance ToTags QL.Date
instance ToTags QL.Result
instance ToTags QL.Dbtype
instance ToTags QL.Min
instance ToTags QL.ReturnType
instance ToTags QL.Desc
instance ToTags QL.Minus
instance ToTags QL.Select
instance ToTags QL.Direction
instance ToTags QL.Mod
instance ToTags QL.SimpleId
instance ToTags QL.Disjunction
instance ToTags QL.Slash
instance ToTags QL.Empty
instance ToTags QL.ModuleAliasBody
instance ToTags QL.SpecialCall
instance ToTags QL.Eq
instance ToTags QL.ModuleExpr
instance ToTags QL.SpecialId
instance ToTags QL.Exists
instance ToTags QL.ModuleMember
instance ToTags QL.Star
instance ToTags QL.ClasslessPredicateCall
