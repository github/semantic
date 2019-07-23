{-# LANGUAGE TypeOperators #-}
module Data.Core.Parser
  ( module Text.Trifecta
  , core
  , lit
  , expr
  , lvalue
  ) where

-- Consult @doc/grammar.md@ for an EBNF grammar.

import           Control.Applicative
import qualified Data.Char as Char
import           Data.Core (Core)
import qualified Data.Core as Core
import           Data.Foldable (foldl')
import           Data.Name
import           Data.String
import           Data.Term
import qualified Text.Parser.Token as Token
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Trifecta hiding (ident)

-- * Identifier styles and derived parsers

validIdentifierStart :: Char -> Bool
validIdentifierStart c = not (Char.isDigit c) && isSimpleCharacter c

coreIdents :: TokenParsing m => IdentifierStyle m
coreIdents = Token.IdentifierStyle
  { _styleName              = "core"
  , _styleStart             = satisfy validIdentifierStart
  , _styleLetter            = satisfy isSimpleCharacter
  , _styleReserved          = reservedNames
  , _styleHighlight         = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = Token.reserve coreIdents

identifier :: (TokenParsing m, Monad m, IsString s) => m s
identifier = choice [quote, plain] <?> "identifier" where
  plain = Token.ident coreIdents
  quote = between (string "#{") (symbol "}") (fromString <$> some (noneOf "{}"))

-- * Parsers (corresponding to EBNF)

core :: (TokenParsing m, Monad m) => m (Term Core User)
core = expr

expr :: (TokenParsing m, Monad m) => m (Term Core User)
expr = assign

assign :: (TokenParsing m, Monad m) => m (Term Core User)
assign = application <**> (flip (Core..=) <$ symbolic '=' <*> application <|> pure id) <?> "assignment"

application :: (TokenParsing m, Monad m) => m (Term Core User)
application = projection `chainl1` (pure (Core.$$))

projection :: (TokenParsing m, Monad m) => m (Term Core User)
projection = foldl' (Core....) <$> atom <*> many (namedValue <$> (dot *> name))

atom :: (TokenParsing m, Monad m) => m (Term Core User)
atom = choice
  [ comp
  , ifthenelse
  , edge
  , lit
  , ident
  , rec
  , parens expr
  ]

comp :: (TokenParsing m, Monad m) => m (Term Core User)
comp = braces (Core.do' <$> sepEndByNonEmpty statement semi) <?> "compound statement"

statement :: (TokenParsing m, Monad m) => m (Maybe (Named User) Core.:<- Term Core User)
statement
  =   try ((Core.:<-) . Just <$> name <* symbol "<-" <*> expr)
  <|> (Nothing Core.:<-) <$> expr
  <?> "statement"

ifthenelse :: (TokenParsing m, Monad m) => m (Term Core User)
ifthenelse = Core.if'
  <$ reserved "if"   <*> expr
  <* reserved "then" <*> expr
  <* reserved "else" <*> expr
  <?> "if-then-else statement"

rec :: (TokenParsing m, Monad m) => m (Term Core User)
rec = Core.rec <$ reserved "rec" <*> name <* symbolic '=' <*> expr <?> "recursive binding"

edge :: (TokenParsing m, Monad m) => m (Term Core User)
edge = Core.load <$ reserved "load" <*> expr

lvalue :: (TokenParsing m, Monad m) => m (Term Core User)
lvalue = choice
  [ projection
  , ident
  , parens expr
  ]

-- * Literals

name :: (TokenParsing m, Monad m) => m (Named User)
name = named' <$> identifier <?> "name"

lit :: (TokenParsing m, Monad m) => m (Term Core User)
lit = let x `given` n = x <$ reserved n in choice
  [ Core.bool True  `given` "#true"
  , Core.bool False `given` "#false"
  , Core.unit       `given` "#unit"
  , record
  , between (string "\"") (string "\"") (Core.string . fromString <$> many ('"' <$ string "\\\"" <|> noneOf "\""))
  , lambda
  ] <?> "literal"

record :: (TokenParsing m, Monad m) => m (Term Core User)
record = Core.record <$ reserved "#record" <*> braces (sepEndBy ((,) <$> identifier <* symbolic ':' <*> expr) comma)

lambda :: (TokenParsing m, Monad m) => m (Term Core User)
lambda = Core.lam <$ lambduh <*> name <* arrow <*> expr <?> "lambda" where
  lambduh = symbolic 'λ' <|> symbolic '\\'
  arrow   = symbol "→"   <|> symbol "->"

ident :: (Monad m, TokenParsing m) => m (Term Core User)
ident = pure . namedValue <$> name <?> "identifier"
