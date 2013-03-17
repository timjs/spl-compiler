{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.SPL.Pretty
  ( Pretty(..) ) where

import Language.SPL.Program

import Text.PrettyPrint.Leijen

tabstop = 4

parensized :: (Pretty a) => [a] -> Doc
parensized = parens . hcat . punctuate (comma <> space) . map pretty

block :: (Pretty a) => a -> Doc
block b = braces $ line <> indent tabstop (pretty b) <> line

instance Pretty Program where
  pretty = sep . map pretty

instance Pretty Declaration where
  pretty (Declare t n e)    = pretty t <+> pretty n <+>
                              equals <+> pretty e <> semi
  pretty (Define t n p d b) = pretty t <+> pretty n <+> pretty p <+>
                              block (pretty d <$> pretty b)

instance Pretty Type where
  pretty VOID       = text "Void"
  pretty INT        = text "Int"
  pretty BOOL       = text "Bool"
  pretty (PAIR s t) = parensized [s,t]
  pretty (LIST t)   = brackets (pretty t)
  pretty (Type s)   = text s

instance Pretty Name where
  pretty Print    = text "print"
  pretty IsEmpty  = text "isEmpty"
  pretty Head     = text "head"
  pretty Tail     = text "tail"
  pretty Fst      = text "fst"
  pretty Snd      = text "snd"
  pretty Main     = text "main"
  pretty (Name s) = text s

instance Pretty Parameter where
  pretty (Parameter t n) = pretty t <+> pretty n

instance Pretty Parameters where
  pretty = parensized

instance Pretty Arguments where
  pretty = parensized

instance Pretty Block where
  pretty = vsep . map pretty

instance Pretty Statement where
  pretty (Assign n e)  = pretty n <+> equals <+> pretty e <> semi
  pretty (Execute n a) = pretty n <> pretty a <> semi
  pretty (Return m)    = text "return" <+> pretty m <> semi
  pretty (If c t e)    = text "if" <+> parens (pretty c) <+> 
                         block t <+> text "else" <+> block e
  pretty (While c l)   = text "while" <+> parens (pretty c) <+> block l

instance Pretty Expression where
  pretty (Variable n)  = pretty n
  pretty (Integer i)   = pretty i
  pretty (Boolean b)   = pretty b
  pretty (Nil)         = text "[]"
  pretty (Pair x y)    = parensized [x,y]
  pretty (Call n as)   = pretty n <> pretty as
  pretty (Infix o l r) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Prefix o e)  = parens (pretty o <> pretty e)

instance Pretty BinaryOperator where
  pretty Add  = char '+'
  pretty Sub  = char '-'
  pretty Mul  = char '*'
  pretty Div  = char '/'
  pretty Mod  = char '%'
  pretty Eq   = text "=="
  pretty Ne   = text "!="
  pretty Lt   = char '<'
  pretty Gt   = char '>'
  pretty Le   = text "<="
  pretty Ge   = text ">="
  pretty And  = text "&&"
  pretty Or   = text "||"
  pretty Cons = char ':'

instance Pretty UnaryOperator where
  pretty Not = char '!'
  pretty Neg = char '-'



{-
instance Monoid Doc where
  mempty  = empty -- empty
  mappend = (<>)  -- <> or ++
  mconcat = hcat  -- concat

space = hsep
lines = vsep
-}
