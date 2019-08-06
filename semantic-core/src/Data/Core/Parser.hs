{-# LANGUAGE FlexibleContexts, TypeOperators #-}
module Data.Core.Parser
  ( module Text.Trifecta
  , core
  , lit
  , expr
  , lvalue
  ) where

-- Consult @doc/grammar.md@ for an EBNF grammar.

import           Control.Applicative
import           Control.Effect.Carrier
import qualified Data.Char as Char
import           Data.Core (Core)
import qualified Data.Core as Core
import           Data.Foldable (foldl')
import           Data.Name
import           Data.String
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

core :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
core = expr

expr :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
expr = ifthenelse <|> lambda <|> rec <|> load <|> assign

assign :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
assign = application <**> (flip (Core..=) <$ symbolic '=' <*> application <|> pure id) <?> "assignment"

application :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
application = projection `chainl1` (pure (Core.$$))

projection :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
projection = foldl' (Core....) <$> atom <*> many (namedValue <$ dot <*> name)

atom :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
atom = choice
  [ comp
  , lit
  , ident
  , parens expr
  ]

comp :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
comp = braces (Core.do' <$> sepEndByNonEmpty statement semi) <?> "compound statement"

statement :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (Maybe (Named Name) Core.:<- t Name)
statement
  =   try ((Core.:<-) . Just <$> name <* symbol "<-" <*> expr)
  <|> (Nothing Core.:<-) <$> expr
  <?> "statement"

ifthenelse :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
ifthenelse = Core.if'
  <$ reserved "if"   <*> expr
  <* reserved "then" <*> expr
  <* reserved "else" <*> expr
  <?> "if-then-else statement"

rec :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
rec = Core.rec <$ reserved "rec" <*> name <* symbolic '=' <*> expr <?> "recursive binding"

load :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
load = Core.load <$ reserved "load" <*> expr

lvalue :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
lvalue = choice
  [ projection
  , ident
  , parens expr
  ]

-- * Literals

name :: (TokenParsing m, Monad m) => m (Named Name)
name = named' <$> identifier <?> "name"

lit :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
lit = let x `given` n = x <$ reserved n in choice
  [ Core.bool True  `given` "#true"
  , Core.bool False `given` "#false"
  , Core.unit       `given` "#unit"
  , record
  , token (between (string "\"") (string "\"") (Core.string . fromString <$> many (escape <|> (noneOf "\"" <?> "non-escaped character"))))
  ] <?> "literal"
  where escape = char '\\' *> choice
          [ '"'  <$ string "\""
          , '\n' <$ string "n"
          , '\r' <$ string "r"
          , '\t' <$ string "t"
          ] <?> "escape sequence"

record :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
record = Core.record <$ reserved "#record" <*> braces (sepEndBy ((,) <$> identifier <* symbolic ':' <*> expr) comma)

lambda :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
lambda = Core.lam <$ lambduh <*> name <* arrow <*> expr <?> "lambda" where
  lambduh = symbolic 'λ' <|> symbolic '\\'
  arrow   = symbol "→"   <|> symbol "->"

ident :: (Applicative t, Monad m, TokenParsing m) => m (t Name)
ident = pure . namedValue <$> name <?> "identifier"
