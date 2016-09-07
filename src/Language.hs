{-# LANGUAGE DataKinds #-}
module Language where

import Data.Record
import Info
import Prologue
import Source
import SourceSpan
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
    ".rb" -> Just Ruby
    _ -> Nothing

termConstructor
  :: Source Char -- ^ The source that the term occurs within.
  -> IO SourceSpan -- ^ The span that the term occupies. This is passed in 'IO' to guarantee some access constraints & encourage its use only when needed (improving performance).
  -> Text -- ^ The name of the production for this node.
  -> Range -- ^ The character range that the term occupies.
  -> [Term Text (Record '[Range, Category])] -- ^ The child nodes of the term.
  -> IO (Term Text (Record '[Range, Category])) -- ^ The resulting term, in IO.
termConstructor source sourceSpan name range children
  | name == "ERROR" = sourceSpan >>= withDefaultInfo . (`S.Error` children)
  | otherwise = withDefaultInfo $ case (name, children) of
  (_, []) -> S.Leaf . toText $ slice range source
  _ -> S.Indexed children
  where withDefaultInfo syntax = pure $! cofree ((range .: Other name .: RNil) :< syntax)
