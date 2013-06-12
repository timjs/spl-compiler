module Language.SPL.Lexer where

import Control.Applicative

import Text.Parsec.Combinator (eof)
import Text.Parsec.Char
import qualified Text.Parsec.Token as T
import qualified Text.Parsec.Language as L

basicNames   = [ "if", "else", "while", "return"
               , "True", "False"
               , "Void", "Int", "Bool"
               , "print", "isEmpty", "head", "tail", "fst", "snd", "main"
               ]
basicSymbols = [ "+", "-", "*", "/", "%"
               , "==", "!=", "<", ">", "<=", ">="
               , "&&", "||", "!", ":"
               ]

splDef = L.emptyDef
  { T.commentStart    = "/*"
  , T.commentEnd      = "*/"
  , T.commentLine     = "//"
  , T.nestedComments  = True
  , T.identStart      = letter
  , T.identLetter     = alphaNum <|> char '_'
  , T.caseSensitive   = True
  , T.reservedNames   = basicNames
  , T.reservedOpNames = basicSymbols
  }

lexer = T.makeTokenParser splDef

lexeme     = T.lexeme     lexer
symbol     = T.symbol     lexer
--whiteSpace = T.whiteSpace lexer
word       = T.identifier lexer
--FIXME: Check case here or later?
--lowercased = (:) <$> lower <*> word
--uppercased = (:) <$> upper <*> word

operator   = T.reservedOp lexer
reserved   = T.reserved   lexer
--integer    = T.integer    lexer
decimal    = lexeme $ T.decimal lexer

parens     = T.parens     lexer
braces     = T.braces     lexer
brackets   = T.brackets   lexer

semi       = T.semi       lexer
semiSep    = T.semiSep    lexer
semiSep1   = T.semiSep1   lexer
comma      = T.comma      lexer
commaSep   = T.commaSep   lexer
commaSep1  = T.commaSep1  lexer

lparen = symbol "("
rparen = symbol ")"
rbrace = symbol "{"
lbrace = symbol "}"
rbrack = symbol "["
lbrack = symbol "]"

paren = lparen <|> rparen
brace = lbrace <|> rbrace
brack = lbrack <|> rbrack
equal = symbol "="

allOf p = T.whiteSpace lexer *> p <* eof

