module Main where

import Control.Monad
import System.Environment
import System.FilePath

import Language.SPL.Parser
import Language.SPL.Printer
import Language.SPL.Analyser
import Language.SPL.Simplifier
import Language.SPL.Translator

import Output

main = do
  as <- getArgs
  when (null as) (error "No arguments given")
  let f = head as
  putAct $ "Parsing " ++ f
  e <- parseSourceFile f
  case e of
    Right p -> do
      putInf "Parsing succeded"
      print p
      print $ pretty p
      putAct "Analysing"
      let (b,r) = (analyse p)
      if b
        then do
          putInf "Analysing succeded"
          putAct "Simplifying"
          let p' = transform p
          print $ pretty p'
          putAct "Compiling"
          let c = compile p'
          print c
          print $ pretty c
          writeFile (replaceExtension f ".ssm") (dullify c)
        else do
          putErr "Analysing failed"
          print $ pretty r
    Left r  -> do
      putErr "Parsing failed"
      print r
  putInf "Goodbye!"
