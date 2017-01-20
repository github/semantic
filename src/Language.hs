{-# LANGUAGE DataKinds, GADTs #-}
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
    _ -> Nothing

termAssignment
  :: Source Char -- ^ The source that the term occurs within.
  -> Record '[Range, Category, SourceSpan] -- ^ The proposed annotation for the term.
  -> [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ The child nodes of the term.
  -> IO [ SyntaxTerm Text '[Range, Category, SourceSpan] ] -- ^ All child nodes (included unnamed productions) of the term as 'IO'. Only use this if you need it.
  -> IO (SyntaxTerm Text '[Range, Category, SourceSpan]) -- ^ The resulting term, in IO.
termAssignment source (range :. category :. sourceSpan :. Nil) children _ =
  withDefaultInfo $ case (category, children) of
    (Error, _) -> S.Error children
    (_, []) -> S.Leaf (toText source)
    _ -> S.Indexed children
  where
    withDefaultInfo syntax =
      pure $! cofree ((range :. category :. sourceSpan :. Nil) :< syntax)

toVarDecl :: (HasField fields Category) => Term (S.Syntax Text) (Record fields) -> Term (S.Syntax Text) (Record fields)
toVarDecl child = cofree $ setCategory (extract child) VarDecl :< S.VarDecl child Nothing

toTuple :: Term (S.Syntax Text) (Record fields) -> [Term (S.Syntax Text) (Record fields)]
toTuple child | S.Indexed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Fixed [key,value] <- unwrap child = [cofree (extract child :< S.Pair key value)]
toTuple child | S.Leaf c <- unwrap child = [cofree (extract child :< S.Comment c)]
toTuple child = pure child
