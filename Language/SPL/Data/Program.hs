{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Language.SPL.Data.Program where

import Language.SPL.Data.Position

import Language.SPL.Printer


-- Types -----------------------------------------------------------------------

type Program    = Constructs
type Constructs = [Construct]

data Construct = Declaration Type Name Expression
               | Definition  Type Name Parameters Constructs Block
               deriving (Show, Eq)

data Type = VOID
          | INT
          | BOOL
          | PAIR Type Type
          | LIST Type
          | POLY String
          deriving (Show, Eq, Ord)
data Name = Print
          | IsEmpty
          | Head
          | Tail
          | Fst
          | Snd
          | Main
          | Name String
          deriving (Show, Eq, Ord)

type Parameters = [Parameter]
data Parameter  = Parameter Type Name
                deriving (Show, Eq, Ord)

type Arguments  = [Argument]
type Argument   = Expression

type Block      = [Statement]

data Statement  = Assign  Name Expression
                | If      Expression Block Block
                | While   Expression Block
                | Return  (Maybe Expression)
                | Execute Name Arguments
                deriving (Show, Eq, Ord)
data Expression = Value    Name
                | Integer  Integer
                | Boolean  Bool
                | Nil
                | Pair     Expression Expression
                | Call     Name Arguments
                | Infix    BinaryOperator Expression Expression
                | Prefix   UnaryOperator Expression
                deriving (Show, Eq, Ord)

data BinaryOperator = Add | Sub | Mul | Div | Mod
                    | Eq | Ne | Lt | Gt | Le | Ge 
                    | And | Or | Cons
                    deriving (Show, Eq, Ord)
data UnaryOperator  = Not | Neg
                    deriving (Show, Eq, Ord)

type Arrity      = Int
type Signature   = [Type]


-- Pretty Printer --------------------------------------------------------------

instance Pretty Program where
  pretty = vsep . map pretty

instance Pretty Construct where
  pretty (Declaration t n e)    = pretty t <+> pretty n <+>
                                  align (equals <+> pretty e) <> semi
  pretty (Definition t n p d b) = empty <$>
                                  pretty t <+> pretty n <+> pretty p <+>
                                  block (pretty d <$> pretty b)

instance Pretty Type where
  pretty VOID       = annotation "Void"
  pretty INT        = annotation "Int"
  pretty BOOL       = annotation "Bool"
  pretty (PAIR s t) = parensized [s,t]
  pretty (LIST t)   = brackets (pretty t)
  pretty (POLY s)   = annotation s

instance Pretty Name where
  pretty Print    = identifier "print"
  pretty IsEmpty  = identifier "isEmpty"
  pretty Head     = identifier "head"
  pretty Tail     = identifier "tail"
  pretty Fst      = identifier "fst"
  pretty Snd      = identifier "snd"
  pretty Main     = identifier "main"
  pretty (Name s) = identifier s

instance Pretty Parameters where
  pretty = parensized

instance Pretty Parameter where
  pretty (Parameter t n) = pretty t <+> pretty n

instance Pretty Arguments where
  pretty = parensized

instance Pretty Block where
  pretty = vsep . map pretty

instance Pretty Statement where
  pretty (Assign n e)  = pretty n <+> equals <+> pretty e <> semi
  pretty (Execute n a) = pretty n <> pretty a <> semi
  pretty (Return m)    = keyword "return" <+> pretty m <> semi
  pretty (If c t e)    = keyword "if" <+> parens (pretty c) <+> 
                         block t <+>
                         keyword "else" <+>
                         block e
  pretty (While c l)   = keyword "while" <+> parens (pretty c) <+>
                         block l

instance Pretty Expression where
  pretty (Value n)     = pretty n
  pretty (Integer i)   = constant i
  pretty (Boolean b)   = constant b
  pretty (Nil)         = constant "[]"
  pretty (Pair x y)    = parensized [x,y]
  pretty (Call n as)   = pretty n <> pretty as
  pretty (Infix o l r) = parens . align $ pretty l <+> pretty o </> pretty r
  pretty (Prefix o e)  = parens $ pretty o <> pretty e

instance Pretty BinaryOperator where
  pretty Add  = operator '+'
  pretty Sub  = operator '-'
  pretty Mul  = operator '*'
  pretty Div  = operator '/'
  pretty Mod  = operator '%'
  pretty Eq   = operator "=="
  pretty Ne   = operator "!="
  pretty Lt   = operator '<'
  pretty Gt   = operator '>'
  pretty Le   = operator "<="
  pretty Ge   = operator ">="
  pretty And  = operator "&&"
  pretty Or   = operator "||"
  pretty Cons = operator ':'

instance Pretty UnaryOperator where
  pretty Not = operator '!'
  pretty Neg = operator '-'

