module Language.SPL.Environment where

import Language.SPL.Program

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Extensions as Map

type Environment = Map Name Info

data Info = Function Type Arrity Signature Scope
          | Variable Type

type Arrity    = Int
type Signature = [Type]
type Scope     = Environment





{-
type Scope       = Environment
type Locals      = Environment

globals :: Program -> Environment
globals = create

locals :: Program -> Construct -> Environment
locals p (Definition _ _ ps cs b) = create cs M./\ create ps M./\ create p

class Extractable a where
  extract :: a -> (Name, Info)
  create  :: [a] -> Environment

  create = M.fromList' . map extract

instance Extractable Construct where
  extract (Declaration t n _)      = (n, Variable t)
  extract (Definition  t n ps _ _) = (n, Function t ps)

instance Extractable Parameter where
  extract (Parameter t n) = (n, Variable t)
-}
