{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import BasicPrelude

startCommand = "\\startHASKELL"
stopCommand = "\\stopHASKELL"

unlit :: Text -> Text
unlit = unlines . concat . scan . lines

scan :: [Text] -> [[Text]]
scan lines = process rest
    where (_,rest) = span (/= startCommand) lines

process :: [Text] -> [[Text]]
process [] = []
process (_:rest) = extract (empty : rest)

extract :: [Text] -> [[Text]]
extract lines = save : scan rest
    where (save,rest) = span (/= stopCommand) lines

main = do
    (_      :: String,
     _      :: String,
     input  :: FilePath,
     output :: FilePath) <- readArgs
    text <- readFile input
    writeFile output $ unlit text

