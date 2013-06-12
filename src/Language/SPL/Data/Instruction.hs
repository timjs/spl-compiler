{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ViewPatterns #-}
module Language.SPL.Data.Instruction where

import Language.SPL.Printer (Pretty,pretty,text,char,vsep,(<>),(<+>))

import Prelude hiding (null)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq,singleton,null,(<|),(><),(|>))
import Data.Foldable (toList)

adjust' :: (a -> a) -> Int -> Seq a -> Seq a
adjust' f i s
  | i < 0     = Seq.adjust f (Seq.length s + i) s
  | otherwise = Seq.adjust f i s

type Comment  = String
type Label    = String
type Size     = Integer
type Offset   = Integer
data Register = PC | SP | MP | RV | GP | NL | R6 | R7
              deriving (Show, Eq, Ord, Enum)

data Instruction = Label :# Instruction
               | Instruction :## Comment

               | ADD | SUB | MUL | DIV | MOD | NEG
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

               | JSR | BSR | RET
               | ANNOTE
               | LDAA | LDRR
               | LDLA | LDSA
               | LINK | UNLINK
               | SWP | SWPR | SWPRR
               deriving (Show, Eq)

type Instructions = Seq Instruction

(#) :: Label -> Instructions -> Instructions
l # os
  | null os   = singleton (l :# NOP)
  | otherwise = adjust' (l :#) 0 os
--l # (toList -> [])   = singleton (l :# NOP)
--l # (toList -> o:os) = l :# o <| os

(##) :: Instructions -> Comment -> Instructions
os ## c
  | null os   = singleton (NOP :## c)
  | otherwise = adjust' (:## c) (-1) os

instance Pretty Instruction where
  pretty (l :#  i) = text l <> char ':' <> pretty i
  pretty (i :## c) = pretty i <> char ';' <+> text c
  pretty (BRA l)   = text "\t\t" <> text "BRA" <+> text l <> text "\t\t"
  pretty (BRT l)   = text "\t\t" <> text "BRT" <+> text l <> text "\t\t"
  pretty (BRF l)   = text "\t\t" <> text "BRF" <+> text l <> text "\t\t"
  pretty (LDC' l)  = text "\t\t" <> text "LDC" <+> text l <> text "\t\t"
  pretty i         = text "\t\t" <> text (show i) <> text "\t\t"

instance Pretty Instructions where
  pretty = vsep . map pretty . toList

