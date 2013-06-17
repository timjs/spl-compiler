{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Language.SPL.Simplifier where

import Language.SPL.Data.Program

--import Data.Sequence (Seq, (|>), (<|), ><))
import Data.List
import Data.Foldable (foldrM)
import Data.Functor

import Control.Monad.Supply

-- Sequences -------------------------------------------------------------------

(<|) :: a -> [a] -> [a]
(<|) = (:)

(|>) :: [a] -> a -> [a]
l |> x = l ++ [x] -- Yes, this is inefficient...

(><) :: [a] -> [a] -> [a]
(><) = (++)

-- Supply Monad ----------------------------------------------------------------

type Supplier = Supply Name

temporaries :: [Name]
temporaries = map Temp $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

transform :: Program -> Program
transform p = evalSupply (transform' p) temporaries

transform' :: Program -> Supplier Program
transform' p = do let (gs,fs) = partition isDeclaration p
                  (cs,is) <- simplify gs
                  fs' <- mapM simplify fs
                  return $ Definition VOID Globals [] cs is : (sort . map update) fs'

update :: Construct -> Construct
update (Definition t n ps cs ss) = Definition t n ps (cs ++ concatMap temps ss) ss

temps :: Statement -> Constructs
temps s = case s of
  Assign (Temp t) _  -> [Declaration VOID (Temp t) (Integer 0)]
  If     _ ts fs     -> concatMap temps ts ++ concatMap temps fs
  While  _ ds        -> concatMap temps ds
  _                  -> []

--updateMain :: Statements -> Construct -> Construct
--updateMain gs (Definition VOID Main [] [] ss) = Definition VOID Main [] [] (gs ++ ss)
--updateMain _  c                               = c

-- Simplifiable ----------------------------------------------------------------
-- * Transform Declarations into ordinary Assign.
-- * Extract global Declarations and the initialization to the Main function.
-- * Make sure Call expressions are only allowed in an (temporary) Assign.

class Simplifiable a b where
  simplify :: a -> Supplier b

instance Simplifiable Construct Construct where
  simplify (Definition t n ps cs ss) = do (cs',is) <- simplify cs
                                          ss'      <- simplify ss
                                          return $ Definition t n ps cs' (is ++ ss')

instance Simplifiable Construct (Construct,Statements) where
  simplify (Declaration t n e)       = do (ss,e') <- simplify e
                                          let c' = Declaration t n (Integer 0)
                                          return (c', ss |> Assign n e')

instance Simplifiable Constructs (Constructs,Statements) where
  simplify cs = do (cs',ss') <- unzip <$> mapM simplify cs
                   return (cs', concat ss')

instance Simplifiable Statement Statements where
  simplify (Assign  n (Call m e)) = do (ss,e') <- simplify e
                                       return $ ss |> Assign n (Call m e')
  simplify (Assign  n e)          = do (ss,e') <- simplify e
                                       return $ ss |> Assign n e'
  simplify (If      c ts fs)      = do (ss,c') <- simplify c
                                       ts'     <- simplify ts
                                       fs'     <- simplify fs
                                       return $ ss |> If c' ts' fs'
  simplify (While   c ds)         = do (ss,c') <- simplify c
                                       ds'     <- simplify ds
                                       return $ ss |> While c' ds'
  simplify (Return  Nothing)      = return [Return Nothing]
  simplify (Return  (Just e))     = do (ss,e') <- simplify e
                                       return $ ss |> Return (Just e')
  simplify (Execute n as)         = do (ss,as') <- simplify as
                                       return $ ss |> Execute n as'

instance Simplifiable Statements Statements where
  simplify ss = concat <$> mapM simplify ss--FIXME: lift map?

instance Simplifiable Expression (Statements, Expression) where
  simplify (Pair    a b)   = do (ss,[a',b']) <- simplify [a,b]
                                return (ss, Pair a' b')
  simplify (Call    n as)  = do (ss,as') <- simplify as
                                t <- supply        
                                return (ss |> Assign t (Call n as'), Value t)
  simplify (Infix   o l r) = do (ss,[l',r']) <- simplify [l,r]
                                return (ss, Infix o l' r')
  simplify (Prefix  o e)   = do (ss,e') <- simplify e
                                return (ss, Prefix o e')
  simplify e               = return ([], e) -- Value, Integer, Boolean, List

instance Simplifiable Expressions (Statements, Expressions) where
  simplify es = foldrM add ([],[]) =<< mapM simplify es
    where add :: (Statements,Expression) -> (Statements,Expressions) -> Supply Name (Statements,Expressions)
          (ss,e) `add` (ss',es')
            | e <-> ss' = return (ss >< ss', e <| es')
            | otherwise = do t <- supply
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
  e <-> ss = all (e <->) ss

