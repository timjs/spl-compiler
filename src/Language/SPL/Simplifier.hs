{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Language.SPL.Simplifier where

import Language.SPL.Printer (Pretty, pretty)
import qualified Language.SPL.Printer as Print

import Language.SPL.Data.Program

--import Data.Sequence (Seq, (|>), (<|), (><))
import Data.List
import Data.Foldable (foldrM)

import Control.Monad.Supply

-- Sequences -------------------------------------------------------------------

(<|) :: a -> [a] -> [a]
(<|) = (:)

(|>) :: [a] -> a -> [a]
l |> x = l ++ [x] -- Yes, this is inefficient...

(><) :: [a] -> [a] -> [a]
(><) = (++)

-- Supply Monad ----------------------------------------------------------------

type Temporary = Name
type Supplier = Supply Temporary

temporaries = map (\s -> Name $ '_':s) $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

transform :: Program -> Program
transform p = evalSupply (transform' p) temporaries

transform' :: Program -> Supplier Program
transform' p = do gs' <- simplify gs
                  fs' <- mapM simplify fs
                  return . sort . map (updateMain gs') $ fs'
               where (gs,fs) = partition isDeclaration p

updateMain :: Statements -> Construct -> Construct
updateMain gs (Definition VOID Main [] [] ss) = Definition VOID Main [] [] (gs ++ ss)
updateMain _  c                               = c

-- Simplifiable ----------------------------------------------------------------
-- * Transform Declarations into ordinary Assign.
-- * Extract global Declarations and the initialization to the Main function.
-- * Make sure Call expressions are only allowed in an (temporary) Assign.

class Simplifiable a b where
  simplify :: a -> Supplier b

instance (Simplifiable a Statements) => Simplifiable [a] Statements where
  simplify as = return . concat =<< mapM simplify as

instance Simplifiable Construct Construct where
  simplify (Definition t n ps cs bs) = simplify bs >>= \bs' ->
                                       simplify cs >>= \cs' ->
                                       return $ Definition t n ps [] (cs' ++ bs')

instance Simplifiable Construct Statements where
  simplify (Declaration t n e)       = simplify e >>= \(ss,e') ->
                                       return $ ss |> Assign n e'

instance Simplifiable Statement Statements where
  simplify (Assign  n (Call m e)) = simplify e  >>= \(ss,e') ->
                                return $ ss |> Assign n (Call m e')
  simplify (Assign  n e)      = simplify e  >>= \(ss,e') ->
                                return $ ss |> Assign n e' 
  simplify (If      c ts fs)  = simplify c  >>= \(ss,c') ->
                                simplify ts >>= \ts' ->
                                simplify fs >>= \fs' ->
                                return $ ss |> If c' ts' fs'
  simplify (While   c ds)     = simplify c  >>= \(ss,c') ->
                                simplify ds >>= \ds' ->
                                return $ ss |> While c' ds'
  simplify (Return  Nothing)  = return $ [Return Nothing]
  simplify (Return  (Just e)) = simplify e  >>= \(ss,e') ->
                                return $ ss |> Return (Just e')
  simplify (Execute n as)     = simplify as >>= \(ss,as') ->
                                return $ ss |> Execute n as'

instance Simplifiable Expression (Statements, Expression) where
  simplify (Pair    a b)   = simplify [a,b] >>= \(ss,[a',b']) ->
                             return (ss, Pair a' b')
  simplify (Call    n as)  = simplify as    >>= \(ss,as') ->
                             supply         >>= \t ->
                             return (ss |> Assign t (Call n as'), Value t)
  simplify (Infix   o l r) = simplify [l,r] >>= \(ss,[l',r']) ->
                             return (ss, Infix o l' r')
  simplify (Prefix  o e)   = simplify e     >>= \(ss,e') ->
                             return (ss, Prefix o e')
  simplify e               = return ([], e) -- Value, Integer, Boolean, List

instance Simplifiable Expressions (Statements, Expressions) where
  simplify es = foldrM add ([],[]) =<< mapM simplify es

add :: (Statements,Expression) -> (Statements,Expressions) -> Supply Temporary (Statements,Expressions)
(ss,e) `add` (ss',es')
  | e <-> ss' = return (ss >< ss', e <| es')
  | otherwise = supply >>= \t ->
                return ((ss |> Assign t e) >< ss', Value t <| es')

-- Commutable ------------------------------------------------------------------
-- * Calls do not commute with any statement because of side-effects.
-- * Container expressions commute with a statement if the contained
--   expressions commute with the statement.
-- * Values and literals trivially commute.

class Commutable a where
  (<->) :: Expression -> a -> Bool

instance Commutable Statement where
  Call _ _    <-> _ = False
  Infix _ l r <-> s = l <-> s && r <-> s
  Prefix _ e  <-> s = e <-> s
  Pair x y    <-> s = x <-> s && y <-> s
  _           <-> _ = True

instance Commutable Statements where
  e <-> ss = and $ map (e <->) ss

