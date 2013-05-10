module Language.SPL.Data.Program where

import Language.SPL.Data.Position

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
