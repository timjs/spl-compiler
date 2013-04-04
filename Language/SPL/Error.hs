module Language.SPL.Error where

import Language.SPL.Position
import Language.SPL.Program

import Data.MultiMap (MultiMap)
--import qualified Data.MultiMap as Multi

type Errors = MultiMap Position Error

data Error = UnmatchedType Type Type
           | UnmatchedReturnType Type Type
           | UndefinedName Name
           | DuplicatedName Name
           | FunctionUsedAsVariable Name
           | VariableUsedAsFunction Name
           deriving (Show, Eq, Ord)

