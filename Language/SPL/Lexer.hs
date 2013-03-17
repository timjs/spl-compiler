module Language.SPL.Lexer where

import Control.Applicative

import Text.Parsec.Combinator (eof)
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

splDef = L.javaStyle
  { T.reservedNames   = basicNames
  , T.reservedOpNames = basicSymbols
  }

lexer = T.makeTokenParser splDef

lexeme     = T.lexeme     lexer
symbol     = T.symbol     lexer
word       = T.identifier lexer
whiteSpace = T.whiteSpace lexer

operator   = T.reservedOp lexer
reserved   = T.reserved   lexer
integer    = T.integer    lexer
decimal    = lexeme $ T.decimal    lexer

parens     = T.parens     lexer
braces     = T.braces     lexer
brackets   = T.brackets   lexer

semi       = T.semi       lexer
comma      = T.comma      lexer
semiSep    = T.semiSep    lexer
commaSep   = T.commaSep   lexer
semiSep1   = T.semiSep1   lexer
commaSep1  = T.commaSep1  lexer

paren = symbol "(" <|> symbol ")"
brace = symbol "{" <|> symbol "}"
brack = symbol "[" <|> symbol "]"
equal = symbol "="

allOf p = T.whiteSpace lexer *> p <* eof

