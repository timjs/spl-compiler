{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Language.SPL.Data.Environment where

import Language.SPL.Data.Program
import Language.SPL.Data.Error
import Language.SPL.Data.Position

import Language.SPL.Printer (Pretty, pretty)
import qualified Language.SPL.Printer as Print

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader

import Data.Map (Map)
import qualified Data.Map as Map

data Info = Function Type Arrity Signature
          | Variable Type
          deriving (Show, Eq, Ord)

type Environment = Map Name Info

info :: (MonadReader Environment m) => Name -> m (Maybe Info)
info n = asks (Map.lookup n)

builtins :: Reporter Environment
builtins = return $ Map.fromList types
    where types = [ (Print,   Function VOID 1 [POLY "a"])
                  , (IsEmpty, Function BOOL 1 [LIST (POLY "a")])
                  , (Head,    Function (POLY "a") 1 [LIST (POLY "a")])
                  , (Tail,    Function (LIST (POLY "a")) 1 [LIST (POLY "a")])
                  , (Fst,     Function (PAIR (POLY "a") (POLY "b")) 1 [POLY "a"])
                  , (Snd,     Function (PAIR (POLY "a") (POLY "b")) 1 [POLY "b"])
                  , (Main,    Function VOID 0 [])
                  ]

globals :: Program -> Reporter Environment
globals p = Map.union <$> builtins <*> create p

locals :: Construct -> Reporter Environment
locals (Declaration _ _ _)       = error "no locals for variable declaration"
locals (Definition  _ _ ps cs _) = Map.union <$> parameters <*> constructs
  where parameters = create ps
        constructs = create cs

introduce :: Name -> Info -> Environment -> Reporter Environment
introduce n i b
  | n `Map.member` b = report p (DuplicatedName n) >> return b
  | otherwise        = return $ Map.insert n i b

class Extractable a where
  extract :: a -> (Name, Info)
  create  :: [a] -> Reporter Environment

  create = foldM intro Map.empty
     where intro b a = let (n,i) = extract a
                       in  introduce n i b

instance Extractable Construct where
  extract (Declaration t n _)      = (n, Variable t)
  extract (Definition  t n ps _ _) = (n, Function t a s)
    where a = length ps
          s = map types ps
          types (Parameter t n) = t

instance Extractable Parameter where
  extract (Parameter t n) = (n, Variable t)

instance Pretty Info where
  pretty = Print.text . show

instance Pretty Environment where
  pretty = Print.vsep . map format . Map.toList
     where format (n,i) = pretty n Print.<> Print.text ": " Print.<> pretty i

