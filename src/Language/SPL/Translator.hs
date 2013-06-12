module Language.SPL.Translator where

import Prelude hiding (EQ,GT,LT)

import Language.SPL.Data.Program
import Language.SPL.Data.Instruction
import Language.SPL.Data.Display

import qualified Data.Map as Map
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
  translate (Definition _ n _ _ ss) = do ss' <- translate ss
                                         return ss'--TODO

instance Translatable Statement where
  translate (Assign n e)      = do e' <- translate e
                                   l  <- location n
                                   case l of Local  i -> return $ e'           |> STL i :## show n
                                             Global i -> return $ e' |> LDR GP |> STA i :## show n
  translate (If c ts fs)      = do t <- supply
                                   let [ifLabel,thenLabel,elseLabel,fiLabel] = map (++ t) ["if","then","else","fi"]
                                   c'  <- translate c
                                   ts' <- translate ts
                                   fs' <- translate fs
                                   return $ ifLabel   # c'            ><
                                                        BRF elseLabel <|
                                            thenLabel # ts'           ><
                                                        BRA fiLabel   <|
                                            elseLabel # fs'           ><
                                            fiLabel   # empty
  translate (While c ds)      = do t <- supply
                                   let [whileLabel,doLabel,odLabel] = map (++ t) ["while","do","od"]
                                   c'  <- translate c
                                   ds' <- translate ds
                                   return $ whileLabel # c'             ><
                                                         BRF odLabel    <|
                                            doLabel    # ds'            ><
                                                         BRA whileLabel <|
                                            odLabel    # empty
  translate (Return Nothing)  = return $ singleton RET
  translate (Return (Just e)) = do e' <- translate e
                                   return $ e' |> RET
  translate (Execute n as)    = do as' <- translate $ reverse as
                                   return $ as' |> LDC' (show n) |> JSR

instance Translatable Expression where
  translate (Value n)       = do l <- location n
                                 case l of Local  i -> return $           singleton (LDL i :## show n)
                                           Global i -> return $ LDR GP <| singleton (LDA i :## show n)
  translate (Integer i)     = return $ singleton (LDC i)
  translate (Boolean True)  = return $ singleton (LDC 1)
  translate (Boolean False) = return $ singleton (LDC (-1))
  translate (List)          = error "List not yet implemented"
  translate (Pair x y)      = do x' <- translate x
                                 y' <- translate y
                                 return $ x' >< y' >< singleton (STMH 2)
  translate (Call n as)     = do as' <- translate $ reverse as
                                 return $ as' |> LDC' (show n) |> JSR
  translate (Infix o l r)   = do l' <- translate l
                                 r' <- translate r
                                 o' <- translate o
                                 return $ l' >< r' >< o'
  translate (Prefix o e)    = do e' <- translate e
                                 o' <- translate o
                                 return $ e' >< o'

instance Translatable BinaryOperator where
  translate Add  = return $ singleton ADD
  translate Sub  = return $ singleton SUB
  translate Mul  = return $ singleton MUL
  translate Div  = return $ singleton DIV
  translate Mod  = return $ singleton MOD
  translate Eq   = return $ singleton EQ
  translate Ne   = return $ singleton NE
  translate Lt   = return $ singleton LT
  translate Gt   = return $ singleton GT
  translate Le   = return $ singleton LE
  translate Ge   = return $ singleton GE
  translate And  = return $ singleton AND
  translate Or   = return $ singleton OR
  translate Cons = return $ singleton (error "Cons not yet implemented")
  --translate = return . singleton . fromEnum . toEnum
  --translate o = fromJust . lookup o $ zip bs os
    --where os = map (return . singleton) [ADD, SUB, MUL, DIV, MOD, EQ, NE, LT, GT, LE, GE, AND, OR, CONS]
          --bs =                          [Add, Sub, Mul, Div, Mod, Eq, Ne, Lt, Gt, Le, Ge, And, Or, Cons]

instance Translatable UnaryOperator where
  translate Not = return $ singleton NOT
  translate Neg = return $ singleton NEG

