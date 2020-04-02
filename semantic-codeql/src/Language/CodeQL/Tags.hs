{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Language.CodeQL.Tags
( ToTags(..)
) where

import           AST.Element
import           AST.Token
import           AST.Traversable1
import           Control.Effect.Reader
import           Control.Effect.Writer
import           Data.Foldable (for_)
import           Data.Text (Text)
import qualified Language.CodeQL.AST as CodeQL
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


instance ToTags CodeQL.Module where
  tags t@CodeQL.Module
    { ann = loc@Loc { byteRange }
    , name = CodeQL.ModuleName { extraChildren = CodeQL.SimpleId { text } }
    } = yieldTag text Module loc byteRange >> gtags t

instance ToTags CodeQL.ClasslessPredicate where
  tags t@CodeQL.ClasslessPredicate
    { ann = loc@Loc { byteRange }
    , name = CodeQL.PredicateName { text }
    } = yieldTag text Function loc byteRange >> gtags t

instance ToTags CodeQL.AritylessPredicateExpr where
  tags t@CodeQL.AritylessPredicateExpr
    { ann = loc@Loc { byteRange }
    , name = CodeQL.LiteralId { text }
    } = yieldTag text Call loc byteRange >> gtags t

instance ToTags CodeQL.Dataclass where
  tags t@CodeQL.Dataclass
    { ann = loc@Loc { byteRange }
    , name = CodeQL.ClassName { text }
    } = yieldTag text Class loc byteRange >> gtags t

instance ToTags CodeQL.MemberPredicate where
  tags t@CodeQL.MemberPredicate
    { ann = loc@Loc { byteRange }
    , name = CodeQL.PredicateName { text }
    } = yieldTag text Method loc byteRange >> gtags t

instance ToTags CodeQL.Datatype where
  tags t@CodeQL.Datatype
    { ann = loc@Loc { byteRange }
    , name = CodeQL.ClassName { text }
    } = yieldTag text Class loc byteRange >> gtags t

instance ToTags CodeQL.DatatypeBranch where
  tags t@CodeQL.DatatypeBranch
    { ann = loc@Loc { byteRange }
    , name = CodeQL.ClassName { text }
    } = yieldTag text Class loc byteRange >> gtags t

instance ToTags CodeQL.ClasslessPredicateCall where
  tags CodeQL.ClasslessPredicateCall
    { extraChildren
    } = for_ extraChildren $ \x -> case x of
          Prj t@CodeQL.AritylessPredicateExpr {} -> tags t
          _                                      -> pure ()

instance ToTags CodeQL.QualifiedRhs where
  tags t@CodeQL.QualifiedRhs
    { ann = loc@Loc { byteRange }
    , name = expr
    } = case expr of
          Just (Prj t@CodeQL.PredicateName { text }) -> yieldTag text Call loc byteRange >> gtags t
          _                                          -> gtags t

instance ToTags CodeQL.HigherOrderTerm
instance ToTags CodeQL.AddExpr
instance ToTags CodeQL.Any
instance ToTags CodeQL.ExprAggregateBody
instance ToTags CodeQL.ModuleName
instance ToTags CodeQL.Strictconcat
instance ToTags CodeQL.Addop
instance ToTags CodeQL.Extends
instance ToTags CodeQL.MulExpr
instance ToTags CodeQL.Strictcount
instance ToTags CodeQL.AggId
instance ToTags CodeQL.As
instance ToTags CodeQL.False
instance ToTags CodeQL.Mulop
instance ToTags CodeQL.Strictsum
instance ToTags CodeQL.Aggregate
instance ToTags CodeQL.AsExpr
instance ToTags CodeQL.Field
instance ToTags CodeQL.Ne
instance ToTags CodeQL.String
instance ToTags CodeQL.AnnotArg
instance ToTags CodeQL.AsExprs
instance ToTags CodeQL.Float
instance ToTags CodeQL.Negation
instance ToTags CodeQL.Sum
instance ToTags CodeQL.AnnotName
instance ToTags CodeQL.Asc
instance ToTags CodeQL.Forall
instance ToTags CodeQL.Newtype
instance ToTags CodeQL.Super
instance ToTags CodeQL.Annotation
instance ToTags CodeQL.Avg
instance ToTags CodeQL.Forex
instance ToTags CodeQL.None
instance ToTags CodeQL.SuperRef
instance ToTags CodeQL.Body
instance ToTags CodeQL.FullAggregateBody
instance ToTags CodeQL.Not
instance ToTags CodeQL.This
instance ToTags CodeQL.Bool
instance ToTags CodeQL.Ge
instance ToTags CodeQL.OrderBy
instance ToTags CodeQL.True
instance ToTags CodeQL.Boolean
instance ToTags CodeQL.Gt
instance ToTags CodeQL.OrderBys
instance ToTags CodeQL.TypeAliasBody
instance ToTags CodeQL.Charpred
instance ToTags CodeQL.ParExpr
instance ToTags CodeQL.TypeExpr
instance ToTags CodeQL.IfTerm
instance ToTags CodeQL.Plus
instance ToTags CodeQL.TypeLiteral
instance ToTags CodeQL.ClassMember
instance ToTags CodeQL.Implication
instance ToTags CodeQL.UnaryExpr
instance ToTags CodeQL.ClassName
instance ToTags CodeQL.Import
instance ToTags CodeQL.PredicateAliasBody
instance ToTags CodeQL.Underscore
instance ToTags CodeQL.Predicate
instance ToTags CodeQL.ImportModuleExpr
instance ToTags CodeQL.PredicateExpr
instance ToTags CodeQL.Unop
instance ToTags CodeQL.Imprt
instance ToTags CodeQL.PredicateName
instance ToTags CodeQL.VarDecl
instance ToTags CodeQL.Closure
instance ToTags CodeQL.In
instance ToTags CodeQL.PrefixCast
instance ToTags CodeQL.VarName
instance ToTags CodeQL.CompTerm
instance ToTags CodeQL.InExpr
instance ToTags CodeQL.Ql
instance ToTags CodeQL.Variable
instance ToTags CodeQL.Compop
instance ToTags CodeQL.InstanceOf
instance ToTags CodeQL.Qldoc
instance ToTags CodeQL.Concat
instance ToTags CodeQL.Instanceof
instance ToTags CodeQL.QualModuleExpr
instance ToTags CodeQL.Conjunction
instance ToTags CodeQL.Integer
instance ToTags CodeQL.QualifiedExpr
instance ToTags CodeQL.Count
instance ToTags CodeQL.Le
instance ToTags CodeQL.Class
instance ToTags CodeQL.Literal
instance ToTags CodeQL.Quantified
instance ToTags CodeQL.LiteralId
instance ToTags CodeQL.Quantifier
instance ToTags CodeQL.Lt
instance ToTags CodeQL.Range
instance ToTags CodeQL.DatatypeBranches
instance ToTags CodeQL.Max
instance ToTags CodeQL.Rank
instance ToTags CodeQL.Date
instance ToTags CodeQL.Result
instance ToTags CodeQL.Dbtype
instance ToTags CodeQL.Min
instance ToTags CodeQL.ReturnType
instance ToTags CodeQL.Desc
instance ToTags CodeQL.Minus
instance ToTags CodeQL.Select
instance ToTags CodeQL.Direction
instance ToTags CodeQL.Mod
instance ToTags CodeQL.SimpleId
instance ToTags CodeQL.Disjunction
instance ToTags CodeQL.Slash
instance ToTags CodeQL.Empty
instance ToTags CodeQL.ModuleAliasBody
instance ToTags CodeQL.SpecialCall
instance ToTags CodeQL.Eq
instance ToTags CodeQL.ModuleExpr
instance ToTags CodeQL.SpecialId
instance ToTags CodeQL.Exists
instance ToTags CodeQL.ModuleMember
instance ToTags CodeQL.Star
