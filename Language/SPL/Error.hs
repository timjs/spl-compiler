{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Language.SPL.Error where

import Language.SPL.Position
import Language.SPL.Program

import Control.Monad.Writer

import Data.MultiMap (MultiMap)
import qualified Data.MultiMap as Multi
import Data.Set (Set)
import qualified Data.Set as Set

import Language.SPL.Printer (Pretty, pretty, text, (<+>), (</>))
import qualified Language.SPL.Printer as Print

type Errors = MultiMap Position Error

data Error = TypeMismatch Type Name Type -- expected type, var name, var type
           | ReturnMismatch Type Name Type
           | VariableNotDeclared Name
           | VariableNotInScope Name
           | FunctionNotInScope Name
           | ArrityMismatch Arrity Name Arrity   -- expected arr, fun name, fun arr
           | UnusedReturnValue Name Type
           | DuplicatedName Name
           | FunctionUsedAsVariable Name
           | VariableUsedAsFunction Name
           | Mistery Type Expression
           deriving (Show, Eq, Ord)

type Reporter = Writer Errors

report :: (MonadWriter Errors m) => Position -> Error -> m Bool
report p e = do
  tell $ Multi.singleton p e
  return False

runReporter = runWriter

instance Pretty Error where
  --pretty (TypeUnexpected t t')
  --  = text "Could not match expected type" <+> pretty t </>
  --    text "with actual type" <+> pretty t'
  pretty (TypeMismatch t n t')
    = text "Expected type" <+> pretty t </>
      text "but" <+> pretty n <+> text "has type" <+> pretty t'
  pretty (ReturnMismatch t n t')
    = text "Expected type" <+> pretty t </>
      text "but function" <+> pretty n <+> text "returns type" <+> pretty t'
  pretty (VariableNotDeclared n)
    = text "Variable" <+> pretty n <+> text "has to be declared before it can be assigned to"
  pretty (VariableNotInScope n)
    = text "Variable" <+> pretty n <+> text "is not in scope"
  pretty (FunctionNotInScope n)
    = text "Function" <+> pretty n <+> text "is not in scope"
  pretty (ArrityMismatch a n a')
    = text "Function" <+> pretty n <+> text "is called with" <+> pretty a <+> text "arguments" </>
      text "but it needs" <+> pretty a'
  pretty (UnusedReturnValue n t)
    = text "Return value of" <+> pretty n <+> text "is not used"
  pretty (DuplicatedName n)
    = text "Name" <+> pretty n <+> text "is multiple defined"
  pretty (FunctionUsedAsVariable n)
    = text "Function name" <+> pretty n <+> text "is used as a variable"
  pretty (VariableUsedAsFunction n)
    = text "Variable name" <+> pretty n <+> text "is used as a function"
  pretty (Mistery t e)
    = text "Some misterious error occured when matching" <+> pretty t </>
      text "to" <+> pretty e

instance Pretty Errors where
  pretty = Print.vsep . map format . Multi.toList
     where format (p,es) = Print.hang 4 (pretty p Print.<$> pretty es)

instance Pretty (Set Error) where
  pretty = Print.vsep . map pretty . Set.toList

