module Language.SPL.Printer
  ( module Text.PrettyPrint.ANSI.Leijen
  , tabstop
  , parensized
  , block
  , keyword
  , constant
  , annotation
  , identifier
  , comment
  , operator
  ) where

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


{-
instance Monoid Doc where
  mempty  = empty -- empty
  mappend = (<>)  -- <> or ++
  mconcat = hcat  -- concat

space = hsep
lines = vsep
-}
