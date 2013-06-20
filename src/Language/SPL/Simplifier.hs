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
  Assign (Temp t) _  -> [Declaration (POLY "t") (Temp t) (Integer 0)]--FIXME: type?
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

{- Statements -
 -}
instance Simplifiable Statement Statements where
  simplify s = case s of
    Assign  n (Call m e) -> do (ss,e') <- simplify e
                               return $ ss |> Assign n (Call m e')
    Assign  n e          -> do (ss,e') <- simplify e
                               return $ ss |> Assign n e'
    If      c ts fs      -> do (ss,c') <- simplify c
                               ts'     <- simplify ts
                               fs'     <- simplify fs
                               return $ ss |> If c' ts' fs'
    While   c ds         -> do (ss,c') <- simplify c
                               ds'     <- simplify ds
                               return $ ss |> While c' ds'
    Match   n cs         -> do cs' <- simplify cs -- :: Expression -> Statement
                               return $ [cs' (Value n)] -- TODO Simplify again!!!
    Return  Nothing      -> return [Return Nothing]
    Return  (Just e)     -> do (ss,e') <- simplify e
                               return $ ss |> Return (Just e')
    Execute n as         -> do (ss,as') <- simplify as
                               return $ ss |> Execute n as'

instance Simplifiable Statements Statements where
  simplify ss = concat <$> mapM simplify ss--FIXME: lift map?

{- Cases -
 -}
instance Simplifiable Case (Expression -> Statement) where
  simplify (Case p ss) = do ss' <- simplify ss
                            p'  <- simplify p -- :: Expression -> (Expression,Statements)
                            let (c,as) = (fst . p', snd . p')
                            return $ \e -> If (c e) (as e ++ ss') []

instance Simplifiable Cases (Expression -> Statement) where
  simplify cs = do fs <- mapM simplify cs
                   return $ foldr1 combine . sequence fs
    where
      combine (If c ss []) s = If c ss [s]

{- Patterns -
 - 
 - Returns a function taking the Name of the matcher
 - and returns an Expression to test in an If Statement
 - and possibly some statements with assignments,
 - to be put in the body of the If.
 -}
instance Simplifiable Pattern (Expression -> (Expression,Statements)) where
  simplify p = case p of
      AnyPattern        -> return $ \_ -> (Boolean True, [])
      IntPattern  i     -> return $ \e -> (Infix Eq e (Integer i), [])
      BoolPattern True  -> return $ \e -> (e, [])
      BoolPattern False -> return $ \e -> (Prefix Not e, [])
      NamePattern n     -> return $ \e -> (Boolean True, [Assign n e])
      ListPattern       -> return $ \e -> (Call IsEmpty [e], [])
      ConsPattern l r   -> do l' <- simplify l -- :: Expression -> (Expression,Statements)
                              r' <- simplify r -- :: Expression -> (Expression,Statements)
                              return $ \e -> l' (Call Head [e]) `combine` r' (Call Tail [e])
      PairPattern l r   -> error "pair pattern matching not yet implemented"
      where
        combine :: (Expression,Statements) -> (Expression,Statements) -> (Expression,Statements)
        combine (e,s) (e',s') = (Infix And e e', s ++ s')

{- Expressions -
 -}
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
