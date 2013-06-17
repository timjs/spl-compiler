{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, ViewPatterns #-}
module Language.SPL.Data.Instruction where

import Language.SPL.Printer (Pretty,pretty,text,char,vsep,fill,indent,(<>),(<+>))

import Prelude hiding (null)

import Data.List ((\\))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq,singleton,null)
import Data.Foldable (toList)

adjust' :: (a -> a) -> Int -> Seq a -> Seq a
adjust' f i s
  | i < 0     = Seq.adjust f (Seq.length s + i) s
  | otherwise = Seq.adjust f i s

type Comment  = String
type Label    = String
type Size     = Int
type Offset   = Int
data Register = PC | SP | MP | HP | RR | GP | R6 | R7
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

                 | LDRR Register Register

                 | JSR | BSR | RET
                 | ANNOTE
                 | LDAA
                 | LDLA | LDSA
                 | LINK | UNLINK
                 | SWP | SWPR | SWPRR
                 deriving (Show, Eq)

type Instructions = Seq Instruction

-- Annotateable ----------------------------------------------------------------

infixl 8 #
infixr 9 ##

class Annotatable a where
  (#)  :: Label -> a -> Instructions
  (##) :: a -> Comment -> Instructions

instance Annotatable Instruction where
  (#)  = singleton . (:#)
  (##) = singleton . (:##)

instance Annotatable Instructions where
  l # is
    | null is   = singleton (l :# NOP)
    | otherwise = adjust' (l :#) 0 is

  is ## c
    | null is   = singleton (NOP :## c)
    | otherwise = adjust' (:## c) (-1) is

-- Pretty Printer --------------------------------------------------------------

tabstop :: Int
tabstop = 16

instance Pretty Instruction where
  pretty (l :#  i) = fill tabstop (text l <> char ':') <> pretty i
  pretty (i :## c) = indent tabstop (pretty i) <> char ';' <+> text c
  pretty i         = fill tabstop (text $ show i \\ ['(', ')', '"', '"', '\'']) -- For () around -1, "" around labels and ' of LDC'

instance Pretty Instructions where
  pretty = vsep . map pretty . toList
