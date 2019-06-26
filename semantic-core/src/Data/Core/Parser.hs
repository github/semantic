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
import           Data.Core
import           Data.Name
import           Data.Semigroup
import           Data.String
import           Data.Text (pack)
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

core :: (TokenParsing m, Monad m) => m (Core Name)
core = expr

expr :: (TokenParsing m, Monad m) => m (Core Name)
expr = atom `chainl1` go where
  go = choice [ (:.) <$ dot
              , (:$) <$ notFollowedBy dot
              ]

atom :: (TokenParsing m, Monad m) => m (Core Name)
atom = choice
  [ comp
  , ifthenelse
  , edge
  , lit
  , ident
  , assign
  , parens expr
  ]

comp :: (TokenParsing m, Monad m) => m (Core Name)
comp = braces (sconcat <$> sepEndByNonEmpty expr semi) <?> "compound statement"

ifthenelse :: (TokenParsing m, Monad m) => m (Core Name)
ifthenelse = If
  <$ reserved "if"   <*> core
  <* reserved "then" <*> core
  <* reserved "else" <*> core
  <?> "if-then-else statement"

assign :: (TokenParsing m, Monad m) => m (Core Name)
assign = (:=) <$> try (lvalue <* symbolic '=') <*> core <?> "assignment"

edge :: (TokenParsing m, Monad m) => m (Core Name)
edge = kw <*> expr where kw = choice [ Edge Lexical <$ reserved "lexical"
                                     , Edge Import  <$ reserved "import"
                                     , Load         <$ reserved "load"
                                     ]

lvalue :: (TokenParsing m, Monad m) => m (Core Name)
lvalue = choice
  [ Let <$ reserved "let" <*> name
  , ident
  , parens expr
  ]

-- * Literals

name :: (TokenParsing m, Monad m) => m Name
name = choice [regular, strpath] <?> "name" where
  regular = User <$> identifier
  strpath = Path . pack <$> between (symbolic '"') (symbolic '"') (some $ noneOf "\"")

lit :: (TokenParsing m, Monad m) => m (Core Name)
lit = let x `given` n = x <$ reserved n in choice
  [ Bool True  `given` "#true"
  , Bool False `given` "#false"
  , Unit       `given` "#unit"
  , Frame      `given` "#frame"
  , lambda
  ] <?> "literal"

lambda :: (TokenParsing m, Monad m) => m (Core Name)
lambda = lam <$ lambduh <*> name <* arrow <*> core <?> "lambda" where
  lambduh = symbolic 'λ' <|> symbolic '\\'
  arrow   = symbol "→"   <|> symbol "->"

ident :: (Monad m, TokenParsing m) => m (Core Name)
ident = Var <$> name <?> "identifier"
