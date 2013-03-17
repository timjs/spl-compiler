module Language.SPL.Program where

import Language.SPL.Position

type Program      = Declarations
type Declarations = [Declaration]

data Declaration = Declare Type Name Expression
                 | Define  Type Name Parameters Declarations Block
                 deriving (Show, Eq)

data Type = VOID
          | INT
          | BOOL
          | PAIR Type Type
          | LIST Type
          | Type String
          deriving (Show, Eq)
data Name = Print
          | IsEmpty
          | Head
          | Tail
          | Fst
          | Snd
          | Main
          | Name String
          deriving (Show, Eq)

data Parameter = Parameter Type Name
               deriving (Show, Eq)

type Parameters = [Parameter]
type Arguments  = [Expression]
type Block      = [Statement]
type Test       = Expression -- Of type BOOL

data Statement  = Assign  Name Expression
                | If      Test Block Block
                | While   Test Block
                | Return  (Maybe Expression)
                | Execute Name Arguments
                deriving (Show, Eq)
data Expression = Variable Name
                | Integer  Integer
                | Boolean  Bool
                | Nil
                | Pair     Expression Expression
                | Call     Name Arguments
                | Infix    BinaryOperator Expression Expression
                | Prefix   UnaryOperator Expression
                deriving (Show, Eq)

-- Identifier <> Name
-- Type <> Annotation

{-
instance Show Expression where
  show (Variable n)  = show n
  show (Integer i)   = show i
  show (Boolean b)   = show b
  show (Nil)         = "[]"
  show (Pair x y)    = "(" ++ show x ++ "," ++ show y ++ ")"
  show (Call n a)    = show n ++ "(" ++ show a ++ ")"
  show (Infix o l r) = "(" ++ show l ++ " " ++ show o ++ " " ++ show r ++ ")"
  show (Prefix o e)  = "(" ++ show o ++ show e ++ ")"
-}

data BinaryOperator = Add | Sub | Mul | Div | Mod
                    | Eq | Ne | Lt | Gt | Le | Ge 
                    | And | Or | Cons
                    deriving (Show, Eq)
data UnaryOperator  = Not | Neg
                    deriving (Show, Eq)

