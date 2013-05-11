module Language.SPL.Translator where

import Language.SPL.Data.Instruction

-- Translatable ----------------------------------------------------------------

class Translatable a where
  translate :: a -> Instructions

