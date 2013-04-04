module Language.SPL.Position 
  ( module Text.Parsec.Pos
  , Position
  ) where

import Text.Parsec.Pos

type Position = SourcePos

