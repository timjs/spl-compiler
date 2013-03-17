module Language.SPL.Expression where

import Control.Applicative

import Language.SPL.Lexer
import Language.SPL.Program

import Text.Parsec.String
import Text.Parsec.Prim ((<?>))
import qualified Text.Parsec.Expr as E

import Data.Functor.Identity (Identity)

expressionBuilder p = E.buildExpressionParser table p
                    <?> "expression"

table = [ [prefix "!" Not, prefix "-" Neg]
        , [infixL "*" Mul, infixL "/" Div, infixL "%" Mod]
        , [infixL "+" Add, infixL "-" Sub]
        , [infixR ":" Cons]
        , [infixN "<=" Le, infixN ">=" Ge, infixN ">" Gt, infixN "<" Lt]
        , [infixN "==" Eq, infixN "!=" Ne]
        , [infixR "&&" And]
        , [infixR "||" Or]
        ]

-- An Operator is a wrapped ParsecT:
type Operator = E.Operator String () Identity

binary :: E.Assoc -> String -> BinaryOperator -> Operator Expression
binary a s o = E.Infix (Infix o <$ operator s) a

unary :: String -> UnaryOperator -> Operator Expression
unary s o = E.Prefix (Prefix o <$ operator s)

infixL = binary E.AssocLeft
infixR = binary E.AssocRight
infixN = binary E.AssocNone
prefix = unary

