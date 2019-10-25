{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeOperators #-}
module Core.Parser
  ( core
  , lit
  , expr
  , record
  , comp
  ) where

-- Consult @doc/grammar.md@ for an EBNF grammar.

import           Control.Applicative
import           Control.Effect.Carrier
import           Control.Monad
import           Core.Core ((:<-) (..), Core)
import qualified Core.Core as Core
import           Core.Name
import qualified Data.Char as Char
import           Data.Foldable (foldl')
import           Data.Function
import           Data.String
import           Text.Parser.LookAhead (LookAheadParsing)
import qualified Text.Parser.Token as Token
import qualified Text.Parser.Token.Highlight as Highlight
import qualified Text.Parser.Token.Style as Style
import           Text.Trifecta hiding (ident)

-- * Identifier styles and derived parsers

newtype CoreParser m a = CoreParser { runCoreParser :: m a }
  deriving (Alternative, Applicative, CharParsing, DeltaParsing, Errable, Functor, LookAheadParsing, Monad, MonadPlus, Parsing)

instance TokenParsing m => TokenParsing (CoreParser m) where
  someSpace = Style.buildSomeSpaceParser (void (satisfy Char.isSpace)) comments
    where comments = Style.CommentStyle "" "" "//" False

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
core = runCoreParser expr

expr :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
expr = ifthenelse <|> lambda <|> rec <|> load <|> assign

assign :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
assign = application <**> (symbolic '=' *> rhs <|> pure id) <?> "assignment"
  where rhs = flip (Core..=) <$> application

application :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
application = projection `chainl1` (pure (Core.$$))

projection :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
projection = foldl' (&) <$> atom <*> many (choice [ flip (Core..?)  <$ symbol ".?" <*> identifier
                                                  , flip (Core....) <$ dot         <*> identifier
                                                  ])

atom :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
atom = choice
  [ comp
  , lit
  , ident
  , parens expr
  ]

comp :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
comp = braces (Core.do' <$> sepEndByNonEmpty statement semi) <?> "compound statement"

statement :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (Maybe (Named Name) :<- t Name)
statement
  =   try ((:<-) . Just <$> name <* symbol "<-" <*> expr)
  <|> (Nothing :<-) <$> expr
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

-- * Literals

name :: (TokenParsing m, Monad m) => m (Named Name)
name = named' <$> identifier <?> "name"

lit :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
lit = let x `given` n = x <$ reserved n in choice
  [ Core.bool True  `given` "#true"
  , Core.bool False `given` "#false"
  , Core.unit       `given` "#unit"
  , record
  , Core.string <$> stringLiteral
  ] <?> "literal"

record :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
record = Core.record <$ reserved "#record" <*> braces (sepEndBy ((,) <$> identifier <* symbolic ':' <*> expr) comma)

lambda :: (TokenParsing m, Carrier sig t, Member Core sig, Monad m) => m (t Name)
lambda = Core.lam <$ lambduh <*> name <* arrow <*> expr <?> "lambda" where
  lambduh = symbolic 'λ' <|> symbolic '\\'
  arrow   = symbol "→"   <|> symbol "->"

ident :: (Applicative t, Monad m, TokenParsing m) => m (t Name)
ident = pure . namedValue <$> name <?> "identifier"
