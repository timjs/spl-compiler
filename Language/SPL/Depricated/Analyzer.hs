{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}--, OverlappingInstances #-}
module Language.SPL.Analyzer where

import Language.SPL.Program
import Language.SPL.Position
--import Language.SPL.Environment

type Error  = (Position,String)
type Errors = [Error]

--report :: (Show a, MonadWriter Errors m) => a -> String -> m ()
--report p m = tell $ [show p ++ ": " ++ m]
--warn
--error
--inform

ask :: Name -> Type
ask = undefined

info :: (MonadReader Environment m) => Name -> m Info
info n = asks (M.lookup n)

class Analysable a where
  -- | Deeply analyse types and usage of object
  analyse :: a -> Bool
  -- | Checks if Type matches the type of the object
  match   :: Type -> a -> Bool

instance (Analysable a) => Analysable [a] where --Holds for Blocks and Programs
  -- 
  analyse = and . map analyse
  -- A Block matches a certain type if all the Return statements evaluate to the same type.
  match   t = and . map (match t)

instance Analysable Construct where
  -- For a Declaration we only have to check the give type matches the initialization expression.
  analyse (Declaration t n e)        = match t e
  -- For a Definition we have to check if the return type matches the types of all the Return expressions in the Block
  -- After that continue analyzing the Block.
  analyse (Definition  t n ps cs bs) = match t bs && analyse bs
  
  -- Constructs match any type.
  match _ _ = True

instance Analysable Statement where
  -- We analyse all the components of a given Statement.
  analyse (Return _)     = True
  analyse (Assign n e)   = match (ask n) e
  analyse (If c ts es)   = match BOOL c && analyse ts && analyse es
  analyse (While c ls)   = match BOOL c && analyse ls
  analyse (Execute n as) = length ps == length as && and (zipWith match ps as) -- Warning if return value not used?
                where ps = [ask n] --parameters $ ask n

  -- Statement matches are only used to ensure the types of the Return statements.
  -- We go on recursively checking the Blocks of If and While statements.
  match VOID (Return Nothing)  = True
  match t    (Return (Just e)) = match t e
  match t    (If _ ts es)      = match t ts && match t es
  match t    (While _ ls)      = match t ls
  match _    _                 = True

instance Analysable Expression where
  -- Not implemented, probably not needed.
  analyse (Call n as) = length ps == length as && and (zipWith match ps as)
             where ps = [ask n]
  analyse _ = True

  --match (Poly a)   _           = True
  match (INT)      (Integer _)   = True
  match (BOOL)     (Boolean _)   = True
  match (LIST _)   (Nil)         = True
  match (PAIR t s) (Pair x y)    = match t x && match s y
  match t          (Value n)     = t == ask n
  match t          (Call n as)   = t == ask n && analyse (Call n as)
  match t          (Infix o l r) = match t o && match t l && match t r
  match t          (Prefix o e)  = match t o && match t e
  match t _ = False

instance Analysable BinaryOperator where
  analyse = undefined

  match (INT)    o = o `elem` [Add, Sub, Mul, Div, Mod]
  match (BOOL)   o = o `elem` [Eq, Ne, Lt, Gt, Le, Ge, And, Or]
  match (LIST _) o = o `elem` [Cons] 

instance Analysable UnaryOperator where
  analyse = undefined

  match (BOOL) o = o `elem` [Not, Neg]

parameters :: Construct -> [Type]
parameters (Definition _ _ ps _ _) = map extract ps
     where extract (Parameter t _) = t
parameters _                       = [] -- error!

