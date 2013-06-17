{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ViewPatterns #-}
module Language.SPL.Data.Instruction where

import Language.SPL.Printer (Pretty,pretty,text,char,vsep,fill,(<>),(<+>))

import Prelude hiding (length)

import Data.List ((\\))

import Data.Sequence (Seq,singleton,length,adjust,viewl,viewr,ViewL(..),ViewR(..),(><))
import Data.Foldable (toList)

adjust' :: (a -> a) -> Int -> Seq a -> Seq a
adjust' f i s
  | i < 0     = adjust f (length s + i) s
  | otherwise = adjust f i s

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

data Instruction  = Instruction [Label] Operation [Comment]
type Instructions = Seq Instruction

instruction :: Operation -> Instruction
instruction o = Instruction [] o []

-- Annotateable ----------------------------------------------------------------

class Annotatable a where
  (#)  :: Label -> a -> Instructions
  (##) :: a -> Comment -> Instructions

instance Annotatable Operation where
  l # o  = l # instruction o
  o ## c =     instruction o ## c

instance Annotatable Instruction where
  l # (Instruction ls o cs)  = singleton $ Instruction (l:ls) o cs
  (Instruction ls o cs) ## c = singleton $ Instruction ls o (c:cs)

instance Annotatable Instructions where
  l # is = case viewl is of
    EmptyL  -> l # NOP
    i :< is -> l # i >< is
  is ## c = case viewr is of
    EmptyR  -> NOP ## c
    is :> i -> is >< i ## c

-- Pretty Printer --------------------------------------------------------------

--instance Pretty Instruction where
  --pretty (l :#  i) = text l <> char ':' <> pretty i
  --pretty (i :## c) = pretty i <> char ';' <+> text c
  --pretty i         = text "\t\t" <> text s <> text "\t\t"
           --where s = show i \\ ['(', ')', '"', '"', '\''] -- For () around -1, "" around labels and ' of LDC'

tabstop :: Int
tabstop = 16

instance Pretty Instruction where
  pretty (Instruction [l] o [c]) = fill tabstop (text l <> char ':') <>
                                   fill tabstop (text s) <>
                                   char ';' <> text c
                         where s = show o \\ ['(', ')', '"', '"', '\''] -- For () around -1, "" around labels and ' of LDC'


instance Pretty Instructions where
  pretty = vsep . map pretty . toList
