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
import           Data.Core (Core, Edge(..))
import qualified Data.Core as Core
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
expr = atom `chainl1` go where
  go = choice [ (Core....) <$ dot
              , (Core.$$)  <$ notFollowedBy dot
              ]

atom :: (TokenParsing m, Monad m) => m (Term Core User)
atom = choice
  [ comp
  , ifthenelse
  , edge
  , lit
  , ident
  , rec
  , assign
  , parens expr
  ]

comp :: (TokenParsing m, Monad m) => m (Term Core User)
comp = braces (Core.block <$> sepEndByNonEmpty expr semi) <?> "compound statement"

ifthenelse :: (TokenParsing m, Monad m) => m (Term Core User)
ifthenelse = Core.if'
  <$ reserved "if"   <*> core
  <* reserved "then" <*> core
  <* reserved "else" <*> core
  <?> "if-then-else statement"

rec :: (TokenParsing m, Monad m) => m (Term Core User)
rec = Core.rec <$ reserved "rec" <*> name <* symbolic '=' <*> core <?> "recursive binding"

assign :: (TokenParsing m, Monad m) => m (Term Core User)
assign = (Core..=) <$> try (lvalue <* symbolic '=') <*> core <?> "assignment"

edge :: (TokenParsing m, Monad m) => m (Term Core User)
edge = kw <*> expr where kw = choice [ Core.edge Lexical <$ reserved "lexical"
                                     , Core.edge Import  <$ reserved "import"
                                     , Core.load         <$ reserved "load"
                                     ]

lvalue :: (TokenParsing m, Monad m) => m (Term Core User)
lvalue = choice
  [ ident
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
record = Core.record <$ reserved "#record" <*> braces (sepEndBy ((,) <$> identifier <* symbolic '=' <*> core) semi)

lambda :: (TokenParsing m, Monad m) => m (Term Core User)
lambda = Core.lam <$ lambduh <*> name <* arrow <*> core <?> "lambda" where
  lambduh = symbolic 'λ' <|> symbolic '\\'
  arrow   = symbol "→"   <|> symbol "->"

ident :: (Monad m, TokenParsing m) => m (Term Core User)
ident = pure . namedValue <$> name <?> "identifier"
