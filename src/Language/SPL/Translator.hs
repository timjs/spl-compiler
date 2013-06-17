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

import Data.Sequence (Seq,empty,singleton,(<|),(><),(|>))

import Control.Monad.Reader
import Control.Monad.Supply

-- Supplier Monad --------------------------------------------------------------
-- The Supplier monad is a Reader of Location from a Display
-- and a Supply of Labels

type Supplier = ReaderT Display (Supply Label)

evalSupplier :: Supplier a -> [Label] -> Display -> a
evalSupplier a ls d = evalSupply (runReaderT a d) ls --TODO

-- Translatable ----------------------------------------------------------------

temporaries = map (\s -> '_':s) $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence

compile :: Program -> Instructions
compile p = evalSupplier (translate p) temporaries Map.empty

class Translatable a where
  translate :: a -> Supplier (Seq Instruction)

instance (Translatable a) => Translatable [a] where
  translate xs = return . foldr1 (><) =<< mapM translate xs

instance Translatable Construct where
  translate c = case c of
    -- Variables declarations are easy
    Declaration _ n _          ->    return $ singleton (LDC 0 ## ("Initialize " ++ show n))
    -- Function definitions more complicated
    Definition _ Globals [] cs ss -> do cs' <- translate cs
                                        ss' <- translate ss
                                        return $ cs' ><
                                                 ss'
    Definition _ Main [] cs ss    -> do cs' <- translate cs
                                        ss' <- translate ss
                                        return $ cs'                      ><
                                                 ss'                      ><
                                                 TRAP 0 ## "Print result" <|
                                                 singleton (HALT   ## "Halt machine") --TODO
    Definition _ n ps cs ss        -> do cs' <- translate cs
                                         ss' <- translate ss
                                         return $ dullify n # LDR MP     <|
                                                              LDRR MP SP <|
                                                  ss'

instance Translatable Statement where
  translate s = case s of
    Assign n e      -> do e' <- translate e
                          l  <- location n
                          case l of Local  i -> return $ e'           |> STL i ## dullify s
                                    Global i -> return $ e' |> LDR GP |> STA i ## dullify s
    If c ts fs      -> do t <- supply
                          let [ifLabel,thenLabel,elseLabel,fiLabel] = map (\l -> "_" ++ l ++ t) ["if","then","else","fi"]
                          c'  <- translate c
                          ts' <- translate ts
                          fs' <- translate fs
                          return $ ifLabel   # c'            ## dullify c ><
                                               BRF elseLabel              <|
                                   thenLabel # ts'                        ><
                                               BRA fiLabel                <|
                                   elseLabel # fs'                        ><
                                   fiLabel   # empty
    While c ds      -> do t <- supply
                          let [whileLabel,doLabel,odLabel] = map (\l -> "_" ++ l ++ t) ["while","do","od"]
                          c'  <- translate c
                          ds' <- translate ds
                          return $ whileLabel # c'             ## dullify c ><
                                                BRF odLabel                 <|
                                   doLabel    # ds'                         ><
                                                BRA whileLabel              <|
                                   odLabel    # empty
    Return Nothing  -> return $ LDRR SP MP    ## "Adjust stack pointer" <|
                                STR MP        ## "Reset mark pointer"   <|
                                singleton RET ## "Return"
    Return (Just e) -> do e' <- translate e
                          rn <- translate (Return Nothing)
                          return $ e'                             ><
                                   STR RR ## "Store return value" <|
                                   rn
    -- We inline basic functions
    Execute Print a -> do a' <- translate a
                          return $ a'     |>
                                   TRAP 0 ## ("Print " ++ dullify a)
    -- Function calls place their own arguments on the stack and remove them afterwards
    Execute n as    -> do as' <- translate $ reverse as
                          return $ as'                            |>
                                   LDC' (dullify n)               |>
                                   JSR               ## dullify s |>
                                   AJS (- length as)

instance Translatable Expression where
  translate (Value n)        = do l <- location n
                                  case l of Local  i -> return $           singleton (LDL i ## (dullify n ++ " (local)"))
                                            Global i -> return $ LDR GP <| singleton (LDA i ## (dullify n ++ " (global)"))
  translate (Integer i)      = return $ singleton (LDC i)
  translate (Boolean True)   = return $ singleton (LDC 0)
  translate (Boolean False)  = return $ singleton (LDC (-1))
  translate (List)           = return $ singleton (LDC 0)
  translate (Pair x y)       = do x' <- translate x
                                  y' <- translate y
                                  return $ x' >< y' >< singleton (STMH 2)
  translate (Call n as)      = do ex <- translate (Execute n as)
                                  return $ ex     |>
                                           LDR RR ## "Load return value"
  translate (Infix Cons l r) = do r' <- translate r
                                  l' <- translate l
                                  return $ r' >< l' >< singleton (STMH 2)
  translate (Infix o l r)    = do l' <- translate l
                                  r' <- translate r
                                  o' <- translate o
                                  return $ l' >< r' >< o'
  translate (Prefix o e)     = do e' <- translate e
                                  o' <- translate o
                                  return $ e' >< o'

instance Translatable BinaryOperator where
  translate o = fromJust . Map.lookup o $ operators
  --translate = return . singleton . fromEnum . toEnum

operators :: Map BinaryOperator (Supplier (Seq Instruction))
operators = Map.fromList $ zip bs is
 where is = map (return . singleton) [ADD, SUB, MUL, DIV, MOD, EQ, NE, LT, GT, LE, GE, AND, OR]
       bs =                          [Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge, And, Or]

instance Translatable UnaryOperator where
  translate Not = return $ singleton NOT
  translate Neg = return $ singleton NEG

