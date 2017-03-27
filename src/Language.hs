{-# LANGUAGE DataKinds #-}
module Language where

import Data.Record
import Info
import Prologue
import qualified Syntax as S
import Term
import Data.List (partition)

-- | A programming language.
data Language =
      C
    | CoffeeScript
    | CPlusPlus
    | CSharp
    | CSS
    | Haskell
    | HTML
    | Java
    | JavaScript
    | Markdown
    | ObjectiveC
    | Perl
    | PHP
    | Python
    | R
    | Ruby
    | Swift
    | TypeScript
    | Go
    deriving (Show)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: Text -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> Just C
    ".c" -> Just C
    ".js" -> Just JavaScript
    ".md" -> Just Markdown
    ".rb" -> Just Ruby
    ".go" -> Just Language.Go
    ".ts" -> Just TypeScript
    _ -> Nothing

toVarDeclOrAssignment :: (HasField fields Category) => Term (S.Syntax Text) (Record fields) -> Term (S.Syntax Text) (Record fields)
toVarDeclOrAssignment child = case unwrap child of
  S.Indexed [child', assignment] -> cofree $ setCategory (extract child) VarAssignment :< S.VarAssignment [child'] assignment
  _ -> toVarDecl child

toVarDecl :: (HasField fields Category) => Term (S.Syntax Text) (Record fields) -> Term (S.Syntax Text) (Record fields)
toVarDecl child = cofree $ setCategory (extract child) VarDecl :< S.VarDecl [child]

toTuple :: Term (S.Syntax Text) (Record fields) -> [Term (S.Syntax Text) (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child

toPublicFieldDefinition :: (HasField fields Category) => [SyntaxTerm Text fields] -> Maybe (S.Syntax Text (SyntaxTerm Text fields))
toPublicFieldDefinition children = case partition (\x -> category (extract x) == Identifier) children of
  (prev, [identifier, assignment]) -> Just $ S.VarAssignment (prev ++ [identifier]) assignment
  (prev, [identifier]) -> Just $ S.VarDecl children
  _ -> Nothing

