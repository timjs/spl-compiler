module Language.SPL.Helpers where

import Data.List

import Language.SPL.Printer (Pretty,Doc,pretty,text,indent,line,(<$>),(</>),(<>),(<+>))
import qualified Language.SPL.Printer as Pretty

import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

-- Debuging

tracePretty :: (Pretty a, Pretty b) => a -> b -> b
tracePretty a b = traceShow (line <> text "--" <+> pretty a <+> text "--" <$> indent 3 (pretty b) <$> text "---") b

{-
trace = Debug.Trace.trace

debug :: Bool
debug = True

tracePrt :: (Pretty a, Pretty b) => a -> b -> b
tracePrt a b = traceShow (a ~~ b) b

(~~) :: (Pretty a, Pretty b) => a -> b -> Doc
a ~~ b = pretty a </> pretty b

putMsg :: (Show a) => [a] -> ()
putMsg ss = trace ("** " ++ (intercalate " " . map show) ss) ()
-}

-- Testing

infixr 1 ?

-- test ? trueVal $ falseVal = if test then trueVal else falseVal
(?) :: Bool -> a -> a -> a
(?) b t f = if b then t else f

-- Maps

(\/) :: (Ord k) => Map k a -> Map k a -> Map k a
(\/) = Map.union

(/\) :: (Ord k) => Map k a -> Map k b -> Map k a
(/\) = Map.intersection
