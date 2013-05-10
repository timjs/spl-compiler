module Main where

import Control.Monad
import System.Environment

import Language.SPL.Parser
import Language.SPL.Printer
import Language.SPL.Analyser

main = do
  as <- getArgs
  when (null as) (error "No arguments given")
  putStrLn ">> Parsing..."
  e <- parseSourceFile (as!!0)
  case e of
    Right p -> do
      --putStrLn ":: Parsing succeded"
      print $ pretty p
      putStrLn ">> Analysing..."
      let (b,r) = (analyse p)
      --putStrLn $ if b then ":: Analysing succeded" else "!! Analysing failed"
      print $ pretty r
    Left r  -> do
      --putStrLn "!! Parsing failed"
      print r
  putStrLn ":: Goodbye!"
