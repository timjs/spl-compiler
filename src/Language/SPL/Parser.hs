module Language.SPL.Parser where

import Control.Applicative
import Data.Functor.Identity (Identity)

import Language.SPL.Lexer
import Language.SPL.Data.Program

import Text.Parsec.String
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim ((<?>), try, parse)
import Text.Parsec.Error (ParseError)
import qualified Text.Parsec.Expr as E

parseSource :: String -> String -> Either ParseError Program
parseSource = parse source

parseSourceFile :: String -> IO (Either ParseError Program)
parseSourceFile file = parseSource file <$> readFile file

source :: Parser Program
source = allOf program

program :: Parser Program
program = many1 construct

construct :: Parser Construct
construct =   annotation >>= \an ->
              identifier >>= \id ->
              Declaration an id <$> (equal *> expression <* semi)
          <|> Definition  an id <$> (parensized parameters)
                              -- We need a try here, otherwise @construct@
                              -- eats our identifier as an annotation
                              <*> (brace *> many (try construct))
                              <*> (many1 statement <* brace)
          <?> "variable or function construct"

annotation :: Parser Type
annotation =   VOID <$  reserved "Void"
           <|> INT  <$  reserved "Int"
           <|> BOOL <$  reserved "Bool"
           <|> PAIR <$> (paren *> annotation) <*> (comma *> annotation <* paren)
           <|> LIST <$> brackets annotation
           <|> POLY <$> word
           <?> "type annotation"

identifier :: Parser Name
identifier =   Print   <$  reserved "print"
           <|> IsEmpty <$  reserved "isEmpty"
           <|> Head    <$  reserved "head"
           <|> Tail    <$  reserved "tail"
           <|> Fst     <$  reserved "fst"
           <|> Snd     <$  reserved "snd"
           <|> Main    <$  reserved "main"
           <|> Name    <$> word
           <?> "identifier"

parensized :: Parser [a] -> Parser [a]
parensized p = lparen *> ([] <$ rparen <|> p <* rparen)

parameters :: Parser Parameters
parameters =   commaSep (Parameter <$> annotation <*> identifier)
           <?> "function parameters"

arguments  :: Parser Arguments
arguments  =   commaSep (expression)
           <?> "function arguments"

block :: Parser Statements
block     =   braces (many statement)
          <|> count 1 statement
          <?> "block"

statement, action :: Parser Statement
statement =   If      <$> (reserved "if" *> parens expression) <*> (block)
                      <*> (option [] (reserved "else" *> block))
          <|> While   <$> (reserved "while" *> parens expression) <*> (block)
          <|> Match   <$> (reserved "match" *> identifier) <*> many cas
          <|> Return  <$> (reserved "return" *> optional expression <* semi)
          <|> action
          <?> "statement"
action    =   identifier >>= \id ->
              Execute id <$> (parensized arguments <* semi)
          <|> Assign  id <$> (equal *> expression <* semi)
          <?> "action"

cas :: Parser Case
cas     = Case <$> (reserved "case" *> parens pattern) <*> block
pattern, patternCons :: Parser Pattern
pattern = patternCons `chainr1` patternOper
patternCons =   Anything          <$  symbol "_"
            <|> ListPattern       <$  symbol "[]"
            <|> BoolPattern True  <$  reserved "True"
            <|> BoolPattern False <$  reserved "False"
            <|> IntPattern        <$> decimal
            <|> PairPattern       <$> (paren *> pattern) <*> (comma *> pattern <* paren)
            <|> NamePattern       <$> identifier
            <?> "pattern"
patternOper :: Parser (Pattern -> Pattern -> Pattern)
patternOper = ConsPattern <$ symbol ":"
--patternOper = symbol ":" >> return ConsPattern
            -- empty
            -- <|> ConsPattern x <$> (symbol ":" *> pattern)
--named   =   identifier >>= \id ->
            --ConsPattern (NamePattern id) <$> (symbol ":" *> pattern)
        -- <|> pure (NamePattern id)

expression, term, group, call :: Parser Expression
expression = expressionBuilder term
term       =   Boolean True  <$ reserved "True"
           <|> Boolean False <$ reserved "False"
           <|> Integer    <$> decimal --TS: or integer???
           <|> List       <$  symbol "[]"
           <|> group
           <|> call
           <?> "term"
group      =   paren *> expression >>= \ex ->
               Pair ex <$> (comma *> expression <* paren)
           <|> paren *> pure ex
           <?> "parenthesized term"
call       =   identifier >>= \id ->
               Call id <$> parensized arguments
           <|> pure (Value id)
           <?> "function call or variable"

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

{-
  do paren
     a <- expression
     comma
     b <- expression
     paren
     return (Pair a b)

  paren >>
  expression >>= \a ->
  comma >>
  expression >>= \b ->
  paren >>
  return (Pair a b)

  pure Pair <*> (paren *> expression) <*> (comma *> expression <* paren)

  Pair <$> (paren *> expression) <*> (comma *> expression <* paren)
-}
