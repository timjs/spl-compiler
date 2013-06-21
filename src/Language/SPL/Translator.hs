{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Language.SPL.Translator where

import Prelude hiding (EQ,GT,LT,concatMap)

import Data.Maybe

import Language.SPL.Printer (dullify)
import Language.SPL.Helpers

import Language.SPL.Data.Program
import Language.SPL.Data.Instruction
import Language.SPL.Data.Display

import qualified Data.Map as Map
import Data.Map (Map)

import Data.Sequence (Seq,empty,singleton,(><))

import Data.Foldable (foldMap)

import Control.Monad.Reader
import Control.Monad.Supply

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
compile p = foldMap normalize $ evalSupplier (translate p) labels (tracePretty ("Globals:") (makeDisplay (head p)))

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
                                        ss' <- local (tracePretty ("Locals for _globals") (makeDisplay c) `Map.union`) $ translate ss--FIXME: use \/ ?
                                        return $ dullify Globals # LDR SP ## "Push current stack pointer"       ><
                                                                   STR R5 ## "and save it to reference globals" ><
                                                                   cs'                                          ><
                                                                   ss'
    Definition _ Main [] cs ss    -> do cs' <- translate cs
                                        ss' <- local (tracePretty ("Locals for main") (makeDisplay c) `Map.union`) $ translate ss--FIXME: use \/ ?
                                        return $ dullify Main # LDR SP ## "Push current stack pointer"   ><
                                                                STR MP ## "and mark this as a new frame" ><
                                                                cs'                                      ><
                                                                ss'                                      ><
                                                                HALT   ## "Halt machine"
    Definition _ n _ cs ss        -> do cs' <- translate cs
                                        ss' <- local (tracePretty ("Locals for",n) (makeDisplay c) `Map.union`) $ translate ss--FIXME: use \/ ?
                                        return $ dullify n # LDR MP     ## "Push and save old mark pointer" ><
                                                             LDRR MP SP ## "Set new mark pointer"           ><
                                                             cs'                                            ><
                                                             ss'

instance Translatable Statement where
  translate s = case s of
    Assign n e      -> do e' <- translate e
                          l  <- location n
                          case l of
                            Local  i -> return $ e'                                 ><
                                                 STL i  ## ("Assign " ++ dullify s)
                            Global i -> return $ e'                                 ><
                                                 LDR R5 ## "Load global pointer"    ><
                                                 STA i  ## ("Assign " ++ dullify s)
    If c ts fs      -> do t <- supply
                          let [ifLabel,thenLabel,elseLabel,fiLabel] = map (\l -> "_" ++ l ++ t) ["if","then","else","fi"]
                          c'  <- translate c
                          ts' <- translate ts
                          fs' <- translate fs
                          return $ ifLabel   # c'                                    ><
                                               BRF elseLabel ## "Skip then if False" ><
                                   thenLabel # ts'                                   ><
                                               BRA fiLabel   ## "Skip else"          ><
                                   elseLabel # fs'                                   ><
                                   fiLabel   # NOP
    While c ds      -> do t <- supply
                          let [whileLabel,doLabel,odLabel] = map (\l -> "_" ++ l ++ t) ["while","do","od"]
                          c'  <- translate c
                          ds' <- translate ds
                          return $ whileLabel # c'             ## dullify c          ><
                                                BRF odLabel    ## "Skip do if false" ><
                                   doLabel    # ds'                                  ><
                                                BRA whileLabel ## "Loop"             ><
                                   odLabel    # NOP
    Return Nothing  -> return $ LDRR SP MP ## "Move stack pointer to top of frame" ><
                                STR MP     ## "Reset mark pointer"                 ><
                                RET        ## "Return"
    Return (Just e) -> do e' <- translate e
                          rn <- translate (Return Nothing)
                          return $ e'                            ><
                                   STR RR ## "Save return value" ><
                                   rn
    -- We inline print
    Execute Print a -> do a' <- translate a
                          return $ a'                                ><
                                   TRAP 0 ## ("Print " ++ dullify a)
    
    -- Function calls place their own arguments on the stack and remove them afterwards
    Execute n as    -> do as' <- translate $ reverse as
                          return $ as'                                                       ><
                                   LDC' (dullify n)  ## ("Push label '" ++ dullify n ++ "'") ><
                                   JSR               ## ("Call '" ++ dullify s ++ "'")       ><
                                   AJS (- length as) ## "Remove arguments"

instance Translatable Expression where
  translate e = case e of
    Value n        -> do l <- location n
                         case l of Local  i -> return $ LDL i  ## ("Local variable '" ++ dullify n ++ "'")
                                   Global i -> return $ LDR R5 ## "Load global pointer" ><
                                                        LDA i  ## ("Global variable '" ++ dullify n ++ "'")
    Integer i      -> return $ LDC i    ## ("Push " ++ dullify e)
    Boolean True   -> return $ LDC (-1) ## ("Push " ++ dullify e)
    Boolean False  -> return $ LDC 0    ## ("Push " ++ dullify e)
    List           -> return $ LDC 0    ## ("Push " ++ dullify e)
    Pair x y       -> do x' <- translate x
                         y' <- translate y
                         return $ x'     ## "Fst of pair"  ><
                                  y'     ## "Snd of pair"  ><
                                  STMH 2 ## "Save in heap"
    -- We inline basic functions
    Call Head a    -> translate $ Call Snd a -- Lists are stored in the as tuples of (tail, head),
    Call Tail a    -> translate $ Call Fst a -- so we can build them up more quickly.
    Call IsEmpty a -> do a' <- translate a
                         return $ a'    ## ("Addres of '" ++ dullify a ++ "'")         ><
                                  LDC 0 ## "Push 0"                                    ><
                                  EQ    ## ("Check if '" ++ dullify a ++ "' is empty")
    Call Fst  a    -> do a' <- translate a
                         return $ a'                                      ><
                                  LDMH 0 2 ## "Push tuple/list from heap" ><
                                  AJS (-1) ## "Remove snd/head"
    Call Snd  a    -> do a' <- translate a
                         return $ a'                                        ><
                                  LDMH 0 2 ## "Push tuple/list from heap"   ><
                                  SWP      ## "Swap fst/tail with snd/head" ><
                                  AJS (-1) ## "Remove fst/tail"
    -- and delegate other callst to Execute
    Call n as      -> do ex <- translate (Execute n as)
                         return $ ex                            ><
                                  LDR RR ## "Load return value"
    Infix Cons l r -> do r' <- translate r
                         l' <- translate l
                         return $ r'     ## "Tail of list" ><
                                  l'     ## "Head of list" ><
                                  STMH 2 ## "Save in heap"
    Infix o l r    -> do l' <- translate l
                         r' <- translate r
                         o' <- translate o
                         return $ l' ><
                                  r' ><
                                  o' ## ("Calculate '" ++ dullify e ++ "'")
    Prefix o a     -> do a' <- translate a
                         o' <- translate o
                         return $ a' ><
                                  o' ## ("Calculate '" ++ dullify e ++ "'")

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

