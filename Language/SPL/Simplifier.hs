{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Language.SPL.Simplifier where

import Language.SPL.Data.Program

import Control.Monad.Supply

-- Supply Monad ----------------------------------------------------------------

temporaries = map ('_' :) $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

simplify :: Program -> Program
simplify p = evalSupply (transform p) temporaries

-- Simplifiable ----------------------------------------------------------------
-- * Extract global Declarations.
-- * Transform Declarations into ordinary Assign.
-- * Replace Call expressions with Execute statements.

class Transformable a where
  transform :: a -> Supply String a

instance Transformable Program where
  transform = undefined

