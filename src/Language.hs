{-# LANGUAGE DataKinds, DeriveGeneric #-}
module Language where

import Data.Record
import Data.String
import Info
import Prologue
import qualified Syntax as S
import Term

-- | A programming language.
data Language =
      C
    | Go
    | Markdown
    | Ruby
    | TypeScript -- ^ Also JavaScript.
    | Python
    deriving (Show, Eq, Read, Generic)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: String -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> Just C
    ".c" -> Just C
    ".md" -> Just Markdown
    ".rb" -> Just Ruby
    ".go" -> Just Language.Go
    ".js" -> Just TypeScript
    ".ts" -> Just TypeScript
    ".tsx" -> Just TypeScript
    ".py" -> Just Python
    _ -> Nothing

toVarDeclOrAssignment :: HasField fields Category => Term (S.Syntax Text) (Record fields) -> Term (S.Syntax Text) (Record fields)
toVarDeclOrAssignment child = case unwrap child of
  S.Indexed [child', assignment] -> cofree $ setCategory (extract child) VarAssignment :< S.VarAssignment [child'] assignment
  S.Indexed [child'] -> cofree $ setCategory (extract child) VarDecl :< S.VarDecl [child']
  S.VarDecl _ -> cofree $ setCategory (extract child) VarDecl :< unwrap child
  S.VarAssignment _ _ -> child
  _ -> toVarDecl child

toVarDecl :: HasField fields Category => Term (S.Syntax Text) (Record fields) -> Term (S.Syntax Text) (Record fields)
toVarDecl child = cofree $ setCategory (extract child) VarDecl :< S.VarDecl [child]

toTuple :: Term (S.Syntax Text) (Record fields) -> [Term (S.Syntax Text) (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child

toPublicFieldDefinition :: HasField fields Category => [SyntaxTerm Text fields] -> Maybe (S.Syntax Text (SyntaxTerm Text fields))
toPublicFieldDefinition children = case break (\x -> category (extract x) == Identifier) children of
  (prev, [identifier, assignment]) -> Just $ S.VarAssignment (prev ++ [identifier]) assignment
  (_, [_]) -> Just $ S.VarDecl children
  _ -> Nothing

toInterface :: HasField fields Category => [SyntaxTerm Text fields] -> Maybe (S.Syntax Text (SyntaxTerm Text fields))
toInterface (id : rest) = case break (\x -> category (extract x) == Other "object_type") rest of
  (clauses, [body]) -> Just $ S.Interface id clauses (toList (unwrap body))
  _ -> Nothing
toInterface _ = Nothing
