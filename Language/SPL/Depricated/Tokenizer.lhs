
\startHASKELL
module Language.SPL.Depricated.Tokenizer where

import Prelude.Alternative
\stopHASKELL

The keywords defined in our language are

\startHASKELL
data Keyword = IF
             | ELSE
             | WHILE
             | RETURN
             | TRUE
             | FALSE
             deriving Show

data Type = VOID
          | INT
          | BOOL
          | Tp Text
          deriving Show

data Identifier = PRINT
                | ISEMPTY
                | HEAD
                | TAIL
                | FST
                | SND
                | Id Text
                deriving Show
\stopHASKELL

For speed and brevity instead of

data Symbol = Sym Text

data Operator = ADD | SUB | MUL | DIV | MOD | NEG
              | EQ | NQ | LT | GT | LE | GE
              | AND | OR | NOT
              | CONS

data Interpunction = ASSIGN | STATEMENT | ITEM

data Grouping = Begin Group
              | End Group
data Group    = Expression
              | Block
              | Item

Later hecodeerd: alle operaties in expressies van AST, haakjes niet meer nodig, interpunctie ook niet.

\startHASKELL
data Symbol = PLUS   | MINUS | STAR | SLASH | PERCENT -- Math
            | EQUAL  | COMMA | SEMI | COLON | BANG    -- Punctuation
            | LPAREN | RPAREN                         -- Brackets
            | LBRACE | RBRACE
            | LBRAKT | RBRAKT
            | LANGLE | RANGLE
            | EQUALS | AMPERSANDS | BARS              -- Double symbols
            | BANGEQ | LANGEQ | RANGEQ                -- Combinations
            deriving Show
\stopHASKELL

data Operator = PLUS | MINUS | TIMES | DIVIDE | PERCENT -- Math
              | COLON  | BANG
              | EQUALS | AMPERSANDS | BARS            -- Double symbols
              | BANGEQ | LANGEQ | RANGEQ              -- Combinations
              deriving Show
data Bracket  = Paren
              | Brace
              | Bracket
              deriving Show
data Punctuation = EQUAL | SEMI | COMMA
              deriving Show

\startHASKELL
data Token = Keyword Keyword
           | Type Type
           | Identifier Identifier
           -- | Name Text
           -- | Symbol Text
           | Symbol Symbol
           -- | Operator Operator
           -- | Open Bracket
           -- | Close Bracket
           -- | Punctuation Punctuation
           | Number Integer
           deriving Show
\stopHASKELL

\startHASKELL
main = print $ Keyword IF
\stopHASKELL

% vim: ft=context spell spl=en cole=1
