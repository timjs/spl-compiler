{-# LANGUAGE FlexibleContexts #-}
module Language.SPL.Data.Display where

import Language.SPL.Data.Program

import Data.Maybe
import Data.List
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

builtins :: Display
builtins = undefined

globals :: Construct -> Display
globals (Definition _ Globals [] cs ss) = Map.fromList variables
  where
    variables = zip (nub . concatMap names $ ss) (map Global [1..])

locals :: Construct -> Display
locals (Definition _ _ ps cs ss) = Map.fromList arguments `Map.union` Map.fromList variables
	where
    arguments = zip (map (\(Parameter t n) -> n) ps) (map (Local . negate) [1..])
    variables = zip (nub . concatMap names $ ss) (map Local [1..])

names :: Statement -> [Name]
names s = case s of
  Assign n _  -> [n]
  If _ ts fs  -> concatMap names ts ++ concatMap names fs
  While _ ds  -> concatMap names ds
  Return _    -> []
  Execute _ _ -> []
