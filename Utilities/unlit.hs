{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import BasicPrelude

startCommand = "\\startHASKELL"
stopCommand = "\\stopHASKELL"

remove :: [Text] -> [Text]
remove []                = []
remove (line:rest)
  | line == startCommand = empty : retain rest
  | otherwise            = empty : remove rest

retain :: [Text] -> [Text]
retain []               = error "unlit: reached eof while retaining codeblock"
retain (line:rest)
  | line == stopCommand = empty : remove rest
  | otherwise           = line  : retain rest

unlit :: Text -> Text -> Text
unlit name text = unlines . check . remove . lines $ text
  where check [] = error "unlit: no definitions found in file"
        check l  = pragma : l
        pragma   = "{-# LINE 1 \"" ++ name ++ "\" #-}"

main = do
    (flag   :: Text,     -- Flag to indicate name for LINE pragma follows (-h in GHC)
     name   :: Text,     -- Name to put in LINE pragma
     input  :: FilePath,
     output :: FilePath) <- readArgs
    text <- readFile input
    writeFile output $ unlit name text

