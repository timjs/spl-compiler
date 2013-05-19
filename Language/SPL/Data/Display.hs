{-# LANGUAGE FlexibleContexts #-}
module Language.SPL.Data.Display where

import Language.SPL.Data.Program

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader

data Location = Local  Integer
              | Global Integer
              deriving (Show, Eq, Ord)

type Display = Map Name Location

location :: (MonadReader Display m) => Name -> m Location
location n = return . fromMaybe (Local 100) =<< asks (Map.lookup n)
--location n = return . fromJust =<< asks (Map.lookup n)

builtins :: Display
builtins = undefined

globals :: Constructs -> Display
globals = undefined

locals :: Constructs -> Display
locals = undefined

