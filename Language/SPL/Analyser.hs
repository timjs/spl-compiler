{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Language.SPL.Analyser where

import Language.SPL.Data.Program
import Language.SPL.Data.Position
import Language.SPL.Data.Environment
import Language.SPL.Data.Error
import Language.SPL.Printer (pretty, (<+>), (</>))
import qualified Language.SPL.Printer as Print

import Control.Applicative

import Control.Monad.Reader
import Control.Monad.Writer

import qualified Data.MultiMap as Multi
import qualified Data.Map as Map
--import qualified Data.Map.Extensions as Map

import Debug.Trace

infixl 5 =~, /~
infix  0 <?>
infixr 2 <&>

-- Analyser Monad --------------------------------------------------------------

-- An Analyser is an Reader of Info from Environment
-- and a Writer of Error to Errors (aka a Reporter)
type AnalyserT = ReaderT Environment
type Analyser  = AnalyserT Reporter
--type Analyser = ReaderT Environment Reporter

--check :: Program -> Errors
--check p = runReaderT (p =~ VOID) Map.empty

runAnalyser :: Analyser a -> Environment -> (a,Errors)
runAnalyser a = runReporter . runReaderT a

(<?>) :: Analyser Bool -> Analyser Bool -> Analyser Bool
c <?> t = c >>= \c' -> if c' then return True else t

(<&>) :: Analyser Bool -> Analyser Bool -> Analyser Bool
(<&>) = liftM2 (&&)

info :: (MonadReader Environment m) => Name -> m (Maybe Info)
info n = asks (Map.lookup n)

analyse :: Program -> (Bool,Errors)
analyse p = runAnalyser (tell es >> check p) gs
      where (gs,es) = runReporter $ globals p
{-runAnalyser (do
              tell es
              e <- ask
              debug (Print.text "** Globals" </> pretty e) (check p)
              ) gs-}

-- Checkable -------------------------------------------------------------------

class Checkable a where
  check :: a -> Analyser Bool

{- Programs and Blocks -
 -
 - Programs and Blocks are a list of constructs, so we check them
 - all and fold the returning list of booleans.
 -}
instance (Checkable a) => Checkable [a] where
  check = liftM and . mapM check

{- Constructs -
 -
 - For a Declaration we only have to check the give type matches
 - the initialization expression.
 - For a Definition we have to check if the return type matches the
 - types of all the Return expressions in the Block. After that we
 - continue analysing the Block itself in a new context: the current
 - one updated with the locals of the definition.
 -}
instance Checkable Construct where
  check   (Declaration t _ e)       = e =~ t
  check d@(Definition  t n _ cs bs) = do
    let (ls,es) = runReporter $ locals d
    tell es
    local (`Map.union` ls) (bs =~ t <&> check cs <&> check bs)
      --ask >>= \e -> debug (Print.text "* Locals for" <+> pretty n </> pretty e)

{- Statements -
 -
 - We check all the components of a given statement.
 -}
instance Checkable Statement where
  check (Return _)     = return True
  check (Assign n e)   = checkAss n e =<< info n
    --info n >>= \i -> debug (Print.text "ass =>" <+> pretty n <> Print.text ": " <> pretty i) (checkAss n e i)
  check (If c ts fs)   = c =~ BOOL <&> check ts <&> check fs
  check (While c ls)   = c =~ BOOL <&> check ls
  check (Execute n as) = checkFun n VOID (length as) as =<< info n
    --info n >>= \i -> debug (Print.text "exe =>" <+> pretty n <> Print.text ": " <> pretty i) (checkFun n VOID (length as) as i)

-- Matchable -------------------------------------------------------------------

{- Checks if Type matches the type of the object and reports about possible errors.
 -}
class Matchable a where
  (=~) :: a -> Type -> Analyser Bool
  (/~) :: a -> Type -> Analyser Bool

  a =~ t = not <$> a /~ t
  a /~ t = not <$> a =~ t

{- Blocks and Arguments -
 -
 - Blocks certain =~ a type if all the Return statments
 - evaluate to the same type.
 - Arguments ceratain =~ a type if each argument
 - (which is an expression) matches the give type.
 -}
instance (Matchable a) => Matchable [a] where
  a =~ t = liftM and $ mapM (=~ t) a

{- Statements -
 -
 - Statement matches are only used to ensure the types of the Return statements.
 - We go on recursively checking the Blocks of If and While statements.
 -}
instance Matchable Statement where
  Return Nothing  =~ VOID = return True
  Return (Just e) =~ t    = e  =~ t
  If     _ ts es  =~ t    = ts =~ t <&> es =~ t
  While  _ ls     =~ t    = ls =~ t
  _               =~ _    = return True

instance Matchable Expression where
  Boolean _      =~ BOOL     = return True
  Integer _      =~ INT      = return True
  Nil            =~ LIST _   = return True
  Pair x y       =~ PAIR t s = x =~ t <&> y =~ s
  Value n        =~ t        = checkVar n t =<< info n
    --info n >>= \i -> debug (Print.text "var =>" <+> pretty n <> Print.text ": " <> pretty i) (checkVar n t i)
  Call n as      =~ t        = checkFun n t (length as) as =<< info n
    --info n >>= \i -> debug (Print.text "cal =>" <+> pretty n <> Print.text ": " <> pretty i) (checkFun n t (length as) as i)
  Infix Cons l r =~ LIST t   = l =~ t <&> r =~ LIST t
  Infix o l r    =~ BOOL
    | o `elem` [Eq, Ne, Lt, Gt, Le, Ge]  = l =~ INT  <&> r =~ INT
    | o `elem` [And, Or]                 = l =~ BOOL <&> r =~ BOOL
  Infix o l r    =~ INT
    | o `elem` [Add, Sub, Mul, Div, Mod] = l =~ INT <&> r =~ INT
  Prefix Not e   =~ BOOL     = e =~ BOOL
  Prefix Neg e   =~ INT      = e =~ INT
  _              =~ POLY _   = return True
  e              =~ t        = report p (Mistery t e)

instance Matchable Type where
  POLY _   =~ _          = return True
  _        =~ POLY _     = return True
  LIST t   =~ LIST t'    = t =~ t'
  PAIR t s =~ PAIR t' s' = t =~ t' <&> s =~ s'
  t        =~ t'
    | t == t'            = return True
    | otherwise          = report p (TypeMismatch t (Name "UNUSED") t)



checkVar :: Name -> Type -> Maybe Info -> Analyser Bool
checkVar n t (Just (Variable t'))    = t =~ t' <?> report p (TypeMismatch t n t')
checkVar n _ (Just (Function _ _ _)) = report p (FunctionUsedAsVariable n)
checkVar n _ (Nothing)               = report p (VariableNotInScope n)

checkFun :: Name -> Type -> Arrity -> Arguments -> Maybe Info -> Analyser Bool
checkFun n _ _ _  (Just (Variable _)) = report p (VariableUsedAsFunction n)
checkFun n t a as (Just (Function t' a' ps))
  = (return (a == a') <?> report p (ArrityMismatch a n a')) <&>
    (t =~ t' <?> report p (ReturnMismatch t n t')) <&>
    (and <$> zipWithM (=~) as ps)
checkFun n _ _ _ (Nothing) = report p (FunctionNotInScope n)
--(VOID =~ t' <?> report p (UnusedReturnValue n t')) <&>

checkAss :: Name -> Expression -> Maybe Info -> Analyser Bool
checkAss _ e (Just (Variable t))     = e =~ t
checkAss n _ (Just (Function _ _ _)) = report p (FunctionUsedAsVariable n)
checkAss n _ (Nothing)               = report p (VariableNotDeclared n)



{-
instance Matchable Info where
  Variable t     =~ t' = t =~ t' <?> report p (TypeUnexpected t t')
  Function t _ _ =~ t' = t =~ t' <?> report p (TypeUnexpected t t')

instance (Matchable a) => Matchable (Maybe a) where
  Just a  =~ t = a =~ t
  Nothing =~ _ = report p (NotInScope (Name undefined))
-}
{-
instance Matchable BinaryOperator where
  match (INT)    o = return $ o `elem` [Add, Sub, Mul, Div, Mod]
  match (BOOL)   o = return $ o `elem` [Eq, Ne, Lt, Gt, Le, Ge, And, Or]
  match (LIST _) o = return $ o `elem` [Cons] 
  match _        _ = return False

instance Matchable UnaryOperator where
  Not =~ BOOL = return True
  Neg =~ BOOL = return True
  match _    _   = return False
-}

