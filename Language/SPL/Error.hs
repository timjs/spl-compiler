{-# LANGUAGE FlexibleContexts #-}
module Language.SPL.Error where

import Language.SPL.Position
import Language.SPL.Program

import Control.Monad.Writer

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as Multi

type Errors = MultiMap Position Error

data Error = UnmatchedType Type Type
           | UnmatchedReturnType Type Type
           | UndefinedName Name
           | DuplicatedName Name
           | FunctionUsedAsVariable Name
           | VariableUsedAsFunction Name
           deriving (Show, Eq, Ord)

type Reporter = Writer Errors

report :: (MonadWriter Errors m) => Position -> Error -> m ()
report p e = tell $ Multi.singleton p e

