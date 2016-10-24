{-# LANGUAGE DataKinds #-}
module Language where

import Data.Record
import Info
import Prologue
import Source
import qualified Syntax as S
import Term

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
    deriving (Show)

-- | Returns a Language based on the file extension (including the ".").
languageForType :: Text -> Maybe Language
languageForType mediaType = case mediaType of
    ".h" -> Just C
    ".c" -> Just C
    ".js" -> Just JavaScript
    ".md" -> Just Markdown
    ".rb" -> Just Ruby
    _ -> Nothing

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])] -- ^ The child nodes of the term.
  -> IO (Term (S.Syntax Text) (Record '[Range, Category, SourceSpan])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children =
  withDefaultInfo $ case (name, children) of
    ("ERROR", _) -> S.Error children
    (_, []) -> S.Leaf (toText $ slice range source)
    _ -> S.Indexed children
  where
    withDefaultInfo syntax = do
      sourceSpan' <- sourceSpan
      pure $! cofree ((range .: Other name .: sourceSpan' .: RNil) :< syntax)


toVarDecl :: (HasField fields Category) => Term (S.Syntax Text) (Record fields) -> Term (S.Syntax Text) (Record fields)
toVarDecl child = cofree $ setCategory (extract child) VarDecl :< S.VarDecl child

-- | Convert a If Term to If Syntax. This handles nested else-if clauses recursively,
-- | and satisfies arbitrarily long else-if clauses.
toElseIf :: Term (S.Syntax Text) (Record fields)
         -> Term (S.Syntax Text) (Record fields)
         -> Term (S.Syntax Text) (Record fields)
         -> S.Syntax Text (Term (S.Syntax Text) (Record fields))
toElseIf expr thenClause elseClause = S.If expr thenClause (elseClause' elseClause)
  where
    elseClause' term = case unwrap term of
      S.If _ _ [] -> [ term ]
      S.If then' else' children -> [ cofree (extract term :< S.If then' else' []) ] <> (elseClause' =<< children)
      _ -> [ term ]

toTuple :: Term (S.Syntax Text) (Record fields) -> [Term (S.Syntax Text) (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child
