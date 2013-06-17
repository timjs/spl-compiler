{-# LANGUAGE FlexibleContexts #-}
module Language.SPL.Data.Display where

import Language.SPL.Data.Program

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader

data Location = Local  Int
              | Global Int
              deriving (Show, Eq, Ord)

type Display = Map Name Location

location :: (MonadReader Display m) => Name -> m Location
location n = return . fromMaybe (Local 100) =<< asks (Map.lookup n)
--location n = return . fromJust =<< asks (Map.lookup n)

makeDisplay :: Construct -> Display
makeDisplay (Definition _ Globals [] cs _) = Map.fromList $ zip (map name cs) (map Global [1..])--FIXME: 0 or 1?
makeDisplay (Definition _ _ ps cs _)       = Map.fromList arguments `Map.union` Map.fromList variables
	where
    arguments = zip (map name ps) (map (Local . negate) [1..])
    variables = zip (map name cs) (map Local [1..])
