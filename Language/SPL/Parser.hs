module Language.SPL.Parser where

import Control.Applicative

import Language.SPL.Lexer
import Language.SPL.Position
import Language.SPL.Program
import Language.SPL.Expression

import Text.Parsec.String
import Text.Parsec.Combinator hiding (optional)
import Text.Parsec.Prim ((<?>), try, parse)
import Text.Parsec.Error (ParseError)

parseSource :: SourceName -> String -> Either ParseError Program
parseSource = parse source

parseSourceFile :: SourceName -> IO (Either ParseError Program)
parseSourceFile name = parseSource name <$> readFile name

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
           <|> Poly <$> word
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

block :: Parser Block
block     =   braces (many statement)
          <|> count 1 statement
          <?> "block"

statement, action :: Parser Statement
statement =   If      <$> (reserved "if" *> parens expression) <*> (block)
                      <*> (option [] (reserved "else" *> block))
          <|> While   <$> (reserved "while" *> parens expression) <*> (block)
          <|> Return  <$> (reserved "return" *> optional expression <* semi)
          <|> action
          <?> "statement"
action    =   identifier >>= \id ->
              Execute id <$> (parensized arguments <* semi)
          <|> Assign  id <$> (equal *> expression <* semi)
          <?> "action"

expression, term, group, call :: Parser Expression
expression = expressionBuilder term
term       =   Boolean True  <$ reserved "True"
           <|> Boolean False <$ reserved "False"
           <|> Integer    <$> decimal --TS: or integer???
           <|> Nil        <$  symbol "[]"
           <|> group
           <|> call
           <?> "term"
group      =   paren *> expression >>= \ex ->
               Pair ex <$> (comma *> expression <* paren)
           <|> paren *> pure ex
           <?> "parenthesized term"
call       =   identifier >>= \id ->
               Call id <$> parensized arguments
           <|> pure (Variable id)
           <?> "function call or variable"

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
