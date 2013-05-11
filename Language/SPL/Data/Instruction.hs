{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Language.SPL.Data.Instruction where

type Comment  = String
type Label    = String
type Size     = Int
type Offset   = Int
data Register = PC | SP | MP | RR | GD | R5 | R6 | R7
              deriving (Show, Eq, Ord, Enum)

data Operation = ADD | SUB | MUL | DIV | MOD | NEG
               | AND | OR  | XOR | NOT
               | EQ | NE | GT | LT | GE | LE
               | LDS Offset | LDL Offset | LDA Offset | LDR Register | LDH Offset | LDC Int
               | STS Offset | STL Offset | STA Offset | STR Register | STH
               | LDMS Size Offset | LDML Size Offset | LDMA Size Offset | LDMH Size Offset
               | STMS Size Offset | STML Size Offset | STMA Size Offset | STMH Size
               | BRA Label | BRT Label | BRF Label
               | AJS Offset
               | NOP
               | HALT | TRAP Int

               | JSR | BSR | RET
               | ANNOTE
               | LDAA | LDRR
               | LDLA | LDSA
               | LINK | UNLINK
               | SWP | SWPR | SWPRR
               deriving (Show, Eq)

type Instruction  = (Maybe Label, Operation, Maybe Comment)
type Instructions = [Instruction]

op :: Operation -> Instruction
op o = (Nothing, o, Nothing)

class Annotatable a where
  (<:>) :: Label -> a -> Instruction
  (<#>) :: a -> Comment -> Instruction

instance Annotatable Operation where
  l <:> o = (Just l, o, Nothing)

  o <#> c = (Nothing, o, Just c)

instance Annotatable Instruction where
  l' <:> (Nothing, o, c) = (Just l', o, c)
  l' <:> (Just l,  o, c) = (Just $ l ++ ", " ++ l', o, c)

  (l, o, Nothing) <#> c' = (l, o, Just c')
  (l, o, Just c)  <#> c' = (l, o, Just $ c ++ ", " ++ c')

