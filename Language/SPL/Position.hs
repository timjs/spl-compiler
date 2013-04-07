module Language.SPL.Position 
  ( module Text.Parsec.Pos
  , Position
  , p
  ) where

import Text.Parsec.Pos

type Position = SourcePos

p = initialPos "filename.spl"

