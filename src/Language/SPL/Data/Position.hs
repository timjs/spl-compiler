{-# LANGUAGE TypeSynonymInstances #-}
module Language.SPL.Data.Position 
  ( module Text.Parsec.Pos
  , Position
  , p
  ) where

import Text.Parsec.Pos

import Text.PrettyPrint.ANSI.Leijen (Pretty, pretty, text)

type Position = SourcePos

p = initialPos "filename.spl"

instance Pretty Position where
  pretty = text . show

