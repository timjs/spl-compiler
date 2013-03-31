{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module Language.SPL.Printer
  ( Pretty(..) ) where

import Language.SPL.Program

import Text.PrettyPrint.ANSI.Leijen

tabstop = 4

parensized :: (Pretty a) => [a] -> Doc
parensized = hang 1 . parens . hcat . punctuate (comma <> softline) . map pretty
--parensized = tupled . map pretty

block :: (Pretty a) => a -> Doc
block b = braces $ line <> indent tabstop (pretty b) <> line

keyword, constant, annotation, identifier, comment, operator :: (Pretty a) => a -> Doc
keyword    = yellow  . pretty
constant   = red     . pretty
annotation = green   . pretty
identifier = cyan    . pretty
comment    = blue    . pretty
operator   = magenta . pretty

instance Pretty Program where
  pretty = sep . map pretty

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
  pretty (Poly s)   = text s

instance Pretty Name where
  pretty Print    = identifier "print"
  pretty IsEmpty  = identifier "isEmpty"
  pretty Head     = identifier "head"
  pretty Tail     = identifier "tail"
  pretty Fst      = identifier "fst"
  pretty Snd      = identifier "snd"
  pretty Main     = identifier "main"
  pretty (Name s) = text s

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
                         block t <+> keyword "else" <+> block e
  pretty (While c l)   = keyword "while" <+> parens (pretty c) <+> block l

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



{-
instance Monoid Doc where
  mempty  = empty -- empty
  mappend = (<>)  -- <> or ++
  mconcat = hcat  -- concat

space = hsep
lines = vsep
-}
