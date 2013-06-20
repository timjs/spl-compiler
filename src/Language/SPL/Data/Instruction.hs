{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ViewPatterns #-}
module Language.SPL.Data.Instruction where

import Language.SPL.Printer (Pretty,pretty,text,char,vsep,indent,fill,(<>),(<+>))

import Prelude hiding (length)

import Data.List ((\\),intercalate)
import Data.Sequence (Seq,singleton,viewl,viewr,ViewL(..),ViewR(..),(><),(<|))
import Data.Foldable (toList)

type Comment  = String
type Label    = String
type Size     = Int
type Offset   = Int
data Register = PC | SP | MP | HP | RR | GP | R6 | R7
              deriving (Show, Eq, Ord, Enum)

data Operation = ADD | SUB | MUL | DIV | MOD | NEG
               | AND | OR  | XOR | NOT
               | EQ | NE | GT | LT | GE | LE

               | LDC Integer | LDC' Label
               | LDS Offset | LDL Offset | LDA Offset | LDR Register | LDH Offset
               | STS Offset | STL Offset | STA Offset | STR Register | STH
               | LDMS Size Offset | LDML Size Offset | LDMA Size Offset | LDMH Offset Size
               | STMS Size Offset | STML Size Offset | STMA Size Offset | STMH Size
               | BRA Label | BRT Label | BRF Label
               | AJS Offset
               | NOP
               | HALT | TRAP Integer

               | LDRR Register Register

               | JSR | BSR | RET
               | ANNOTE
               | LDAA
               | LDLA | LDSA
               | LINK | UNLINK
               | SWP | SWPR | SWPRR
               deriving (Show, Eq)

-- Important lesson: use seperate type for such annotations!
data Instruction  = Instruction [Label] Operation [Comment]
                  deriving (Show,Eq)
type Instructions = Seq Instruction

instruction :: Operation -> Instruction
instruction o = Instruction [] o []

-- Annotateable ----------------------------------------------------------------

class Annotatable a where
  (#)  :: Label -> a -> Instructions
  (##) :: a -> Comment -> Instructions

instance Annotatable Operation where
  l # o  = singleton $ Instruction [l] o []
  o ## c = singleton $ Instruction []  o [c]

instance Annotatable Instruction where
  l # (Instruction ls o cs)  = singleton $ Instruction (l:ls) o cs
  (Instruction ls o cs) ## c = singleton $ Instruction ls o (c:cs)

instance Annotatable Instructions where
  l # (viewl -> EmptyL)  = l # NOP
  l # (viewl -> i :< is) = l # i >< is

  (viewr -> EmptyR)  ## c = NOP ## c
  (viewr -> is :> i) ## c = is >< i ## c

normalize :: Instruction -> Instructions
normalize (Instruction (l:l':ls) o cs) = Instruction [l] NOP [] <| normalize (Instruction (l':ls) o cs)
normalize (Instruction ls o cs)        = singleton $ Instruction ls o [intercalate ", " cs]


-- Pretty Printer --------------------------------------------------------------

tabstop :: Int
tabstop = 16

instance Pretty Operation where
  pretty o = text $ show o \\ ['(', ')', '"', '"', '\''] -- For () around -1, "" around labels and ' of LDC'

instance Pretty Instruction where
  pretty (Instruction []  o [])  = indent tabstop (pretty o)
  pretty (Instruction [l] o [])  = fill tabstop (text l <> char ':') <>
                                   pretty o
  pretty (Instruction []  o [c]) = indent tabstop (fill tabstop (pretty o)) <>
                                   char ';' <+> text c
  pretty (Instruction [l] o [c]) = fill tabstop (text l <> char ':') <>
                                   fill tabstop (pretty o) <>
                                   char ';' <+> text c
  pretty i                       = error $ "multiple annotations in instruction not implemented, use normalize: " ++ show i

instance Pretty Instructions where
  pretty = vsep . map pretty . toList
