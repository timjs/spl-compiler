{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Language.SPL.Translator where

import Prelude hiding (EQ,GT,LT)

import Data.Maybe

import Language.SPL.Printer (dullify)

import Language.SPL.Data.Program
import Language.SPL.Data.Instruction
import Language.SPL.Data.Display

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Sequence (Seq,empty,singleton,(><))

import Control.Monad.Reader
import Control.Monad.Supply

import Debug.Trace

traceM :: (Show a, Monad m) => String -> a -> m a
traceM s x = trace (s ++ show x) (return x)

traceS :: (Show a) => String -> a -> a
traceS s x = trace (s ++ show x) x

-- Locator Monad --------------------------------------------------------------
-- The Locator monad is a Reader of Location from a Display
-- and a Supply of Labels

type Locator = ReaderT Display (Supply Label)

evalSupplier :: Locator a -> [Label] -> Display -> a
evalSupplier a ls d = evalSupply (runReaderT a d) ls --TODO

-- Translatable ----------------------------------------------------------------

labels :: [String]
labels = map ('_':) $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

compile :: Program -> Instructions
compile p = evalSupplier (translate p) labels (traceS "** globals: " (makeDisplay $ head p))

class Translatable a where
  translate :: a -> Locator (Seq Instruction)

instance (Translatable a) => Translatable [a] where
  translate [] = return empty
  translate xs = return . foldr1 (><) =<< mapM translate xs

instance Translatable Construct where
  translate c = case c of
    -- Variables declarations are easy
    Declaration _ n _             -> return $ LDC 0 ## ("Initialize " ++ dullify n)
    -- Function definitions are more complicated
    Definition _ Globals [] cs ss -> do cs' <- translate cs
                                        globals <- ask
                                        traceM "** locals for _globals: " $ makeDisplay c `Map.union` globals
                                        ss' <- local (makeDisplay c `Map.union`) $ translate ss--FIXME: use \/ ?
                                        return $ dullify Globals # LDR SP ## "Load current stack pointer..."       ><
                                                                   STR GP ## "...and save it to reference globals" ><
                                                                   cs'                                             ><
                                                                   ss'
    Definition _ Main [] cs ss    -> do cs' <- translate cs
                                        globals <- ask
                                        traceM "** locals for main: " $ makeDisplay c `Map.union` globals
                                        ss' <- local (makeDisplay c `Map.union`) $ translate ss--FIXME: use \/ ?
                                        return $ dullify Main # LDR SP ## "Load current stack pointer..."            ><
                                                                STR MP ## "...and mark this as a new local function" ><
                                                                cs'                                                  ><
                                                                ss'                                                  ><
                                                                HALT   ## "Halt machine"
    Definition _ n _ cs ss        -> do cs' <- translate cs
                                        globals <- ask
                                        traceM ("** locals for " ++ show n) $ makeDisplay c `Map.union` globals
                                        ss' <- local (makeDisplay c `Map.union`) $ translate ss--FIXME: use \/ ?
                                        return $ dullify n # LDR MP                               ><
                                                             LDRR MP SP ## "Set new mark pointer" ><
                                                             cs'                                  ><
                                                             ss'

instance Translatable Statement where
  translate s = case s of
    Assign n e      -> do e' <- translate e
                          l  <- location n
                          case l of
                            Local  i -> return $ e'                 ><
                                                 STL i ## dullify s
                            Global i -> return $ e'                              ><
                                                 LDR GP ## "Load global pointer" ><
                                                 STA i  ## dullify s
    If c ts fs      -> do t <- supply
                          let [ifLabel,thenLabel,elseLabel,fiLabel] = map (\l -> "_" ++ l ++ t) ["if","then","else","fi"]
                          c'  <- translate c
                          ts' <- translate ts
                          fs' <- translate fs
                          return $ ifLabel   # c'            ## dullify c   ><
                                               BRF elseLabel ## "Skip then" ><
                                   thenLabel # ts'                          ><
                                               BRA fiLabel   ## "Skip else" ><
                                   elseLabel # fs'                          ><
                                   fiLabel   # NOP
    While c ds      -> do t <- supply
                          let [whileLabel,doLabel,odLabel] = map (\l -> "_" ++ l ++ t) ["while","do","od"]
                          c'  <- translate c
                          ds' <- translate ds
                          return $ whileLabel # c'             ## dullify c ><
                                                BRF odLabel    ## "Skip do" ><
                                   doLabel    # ds'                         ><
                                                BRA whileLabel ## "Loop"    ><
                                   odLabel    # NOP
    Return Nothing  -> return $ LDRR SP MP    ## "Adjust stack pointer" ><
                                STR MP        ## "Reset mark pointer"   ><
                                RET ## "Return"
    Return (Just e) -> do e' <- translate e
                          rn <- translate (Return Nothing)
                          return $ e'                             ><
                                   STR RR ## "Store return value" ><
                                   rn
    -- We inline basic functions
    Execute Print a -> do a' <- translate a
                          return $ a'     ><
                                   TRAP 0 ## ("Print " ++ dullify a)
    -- Function calls place their own arguments on the stack and remove them afterwards
    Execute n as    -> do as' <- translate $ reverse as
                          return $ as'                                     ><
                                   LDC' (dullify n)  ## "Load label"       ><
                                   JSR               ## dullify s          ><
                                   AJS (- length as) ## "Remove arguments"

instance Translatable Expression where
  translate e = case e of
    Value n        -> do l <- location n
                         case l of Local  i -> return $ LDL i ## (dullify n ++ " (local)")
                                   Global i -> return $ LDR GP ## "Load global pointer" ><
                                                        LDA i  ## (dullify n ++ " (global)")
    Integer i      -> return $ LDC i ## dullify e
    Boolean True   -> return $ LDC 0 ## dullify e
    Boolean False  -> return $ LDC (-1) ## dullify e
    List           -> return $ LDC 0 ## dullify e
    Pair x y       -> do x' <- translate x
                         y' <- translate y
                         return $ x'                       ><
                                  y'                       ><
                                  STMH 2 ## "Save in heap"
    Call n as      -> do ex <- translate (Execute n as)
                         return $ ex     ><
                                  LDR RR ## "Load return value"
    Infix Cons l r -> do r' <- translate r
                         l' <- translate l
                         return $ r'                       ><
                                  l'                       ><
                                  STMH 2 ## "Save in heap"
    Infix o l r    -> do l' <- translate l
                         r' <- translate r
                         o' <- translate o
                         return $ l' >< r' >< o'
    Prefix o a     -> do a' <- translate a
                         o' <- translate o
                         return $ a' >< o'

instance Translatable BinaryOperator where
  translate o = fromJust . Map.lookup o $ operators
  --translate = return . singleton . fromEnum . toEnum

operators :: Map BinaryOperator (Locator (Seq Instruction))
operators = Map.fromList $ zip bs is
 where is = map (return . singleton . instruction) [ADD, SUB, MUL, DIV, MOD, EQ, NE, LT, GT, LE, GE, AND, OR]
       bs =                                        [Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge, And, Or]

instance Translatable UnaryOperator where
  translate Not = return . singleton . instruction $ NOT
  translate Neg = return . singleton . instruction $ NEG

