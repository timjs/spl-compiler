module Language.SPL.Binding where

import Language.SPL.Program
import Language.SPL.Error
import Language.SPL.Position

import Control.Applicative
import Control.Monad
import Control.Monad.Writer

import Data.Map (Map)
import qualified Data.Map as Map

data Binding = Function Type Arrity Signature
             | Variable Type
             deriving (Show, Eq, Ord)

type Bindings = Map Name Binding

type Arrity      = Int
type Signature   = [Type]

globals :: Program -> Reporter Bindings
globals = create

locals :: Construct -> Reporter Bindings
locals (Declaration _ _ _)       = error "no locals for variable declaration"
locals (Definition  _ _ ps cs _) = Map.union <$> parameters <*> constructs
  where parameters = create ps
        constructs = create cs

introduce :: Name -> Binding -> Bindings -> Reporter Bindings
introduce n i b
  | n `Map.member` b = report p (DuplicatedName n) >> return b
  | otherwise        = return $ Map.insert n i b

class Extractable a where
  extract :: a -> (Name, Binding)
  create  :: [a] -> Reporter Bindings

  create = foldM intro Map.empty
     where intro b a = let (n,i) = extract a
                       in  introduce n i b

instance Extractable Construct where
  extract (Declaration t n _)      = (n, Variable t)
  extract (Definition  t n ps _ _) = (n, Function t a s)
    where a = length ps
          s = map types ps
          types (Parameter t n) = t

instance Extractable Parameter where
  extract (Parameter t n) = (n, Variable t)

