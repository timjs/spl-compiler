{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}--, OverlappingInstances #-}
module Language.SPL.Analyser where

import Language.SPL.Program
import Language.SPL.Position
import Language.SPL.Environment
import Language.SPL.Error

import Control.Applicative

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.MultiMap as Multi
import qualified Data.Map as Map
--import qualified Data.Map.Extensions as Map

-- An Analyser is an Reader of Info from Environments
-- and a Writer of Error to Errors
type Analyser = ReaderT Environment (WriterT Errors Identity)

p = initialPos "bla"

report :: Position -> Error -> Analyser ()
report p e = tell $ Multi.singleton p e

infix 0 <?>
(<?>) :: Bool -> Analyser a -> Analyser Bool
True  <?> _ =      return True
False <?> a = a >> return False

--info :: (MonadReader Environment m) => Name -> m Info
info :: Name -> Analyser (Maybe Info)
info n = asks (Map.lookup n)

class Analysable a where
  -- | Deeply analyse types and usage of object
  analyse :: a -> Analyser ()
  -- | Checks if Type matches the type of the object
  match   :: Type -> a -> Analyser Bool

instance (Analysable a) => Analysable [a] where --Holds for Blocks and Programs
  -- 
  analyse = mapM_ analyse
  -- A Block matches a certain type if all the Return statements evaluate to the same type.
  match t = liftM and . mapM (match t)

instance Analysable Construct where
  -- For a Declaration we only have to check the give type matches the initialization expression.
  analyse (Declaration t n e)        = void $ match t e
  -- For a Definition we have to check if the return type matches the types of all the Return expressions in the Block
  -- After that continue analyzing the Block.
  analyse (Definition  t n ps cs bs) = match t bs >> analyse bs
  
  -- Constructs match any type.
  match _ _ = return True

instance Analysable Statement where
  analyse = undefined
  match = undefined
{-
  -- We analyse all the components of a given Statement.
  analyse (Return _)     = return ()
  analyse (Assign n e)   = match (info n) e
  analyse (If c ts es)   = match BOOL c >> analyse ts >> analyse es
  analyse (While c ls)   = match BOOL c >> analyse ls
  analyse (Execute n as) = tell ["execute"] >> return (length ps == length as) >> liftM and (zipWithM match ps as) -- Warning if return value not used?
                where ps = [getSomething n] --parameters $ getSomething n

  -- Statement matches are only used to ensure the types of the Return statements.
  -- We go on recursively checking the Blocks of If and While statements.
  match VOID (Return Nothing)  = return True
  match t    (Return (Just e)) = match t e
  match t    (If _ ts es)      = match t ts >> match t es
  match t    (While _ ls)      = match t ls
  match _    _                 = return True
-}

instance Analysable Expression where
  -- Not implemented, probably not needed.
  --analyse (Call n as) = tell ["call"] >> return (length ps == length as) >> liftM and (zipWithM match ps as)
             --where ps = [getSomething n]
  --return (t == getSomething n) >> analyse (Call n as) Wat doen we hier mee???
  analyse = undefined


  --match (Poly a)   _           = return True
  match (INT)      (Integer _)   = return True
  match (BOOL)     (Boolean _)   = return True
  match (LIST _)   (Nil)         = return True
  match (PAIR t s) (Pair x y)    = (&&) <$> match t x <*> match s y
  match t          (Value n)     = do
    mi <- info n
    case mi of
      Just (Variable t')      -> t == t' <?> report p (UnmatchedType t t')
      Just (Function _ _ _ _) -> False   <?> report p (FunctionUsedAsVariable n)
      Nothing                 -> False   <?> report p (UndefinedName n)
  match t          (Call n as)   = do
    mi <- info n
    case mi of
      Just (Variable _)        -> False   <?> report p (VariableUsedAsFunction n)
      Just (Function t' _ _ _) -> t == t' <?> report p (UnmatchedReturnType t t')
      Nothing                  -> False   <?> report p (UndefinedName n)
  match t          (Infix o l r) = match t o >> match t l >> match t r
  match t          (Prefix o e)  = match t o >> match t e
  match t _ = return False

instance Analysable BinaryOperator where
  analyse = undefined

  match (INT)    o = return $ o `elem` [Add, Sub, Mul, Div, Mod]
  match (BOOL)   o = return $ o `elem` [Eq, Ne, Lt, Gt, Le, Ge, And, Or]
  match (LIST _) o = return $ o `elem` [Cons] 
  match _        _ = return False

instance Analysable UnaryOperator where
  analyse = undefined

  match BOOL Not = return True
  match BOOL Neg = return True
  match _    _   = return False

parameters :: Construct -> [Type]
parameters (Definition _ _ ps _ _) = map extract ps
     where extract (Parameter t _) = t
parameters _                       = [] -- error!

-- <?> ["Could not match expected type " ++ show t ++ " with actual type " ++ show t']
