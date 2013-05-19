module Main where

import Control.Monad
import System.Environment

import Language.SPL.Parser
import Language.SPL.Printer
import Language.SPL.Analyser
import Language.SPL.Simplifier
import Language.SPL.Translator

import Utilities.Output

main = do
  as <- getArgs
  when (null as) (error "No arguments given")
  putAct "Parsing"
  e <- parseSourceFile (as!!0)
  case e of
    Right p -> do
      putInf "Parsing succeded"
      print $ pretty p
      putAct "Analysing"
      let (b,r) = (analyse p)
      case b of
        True  -> do
          putInf "Analysing succeded"
          putAct "Simplifying"
          let p' = transform p
          print $ pretty p'
          putAct "Compiling"
          let c = compile p'
          print $ pretty c
        False -> do
          putErr "Analysing failed"
          print $ pretty r
    Left r  -> do
      putErr "Parsing failed"
      print r
  putInf "Goodbye!"
