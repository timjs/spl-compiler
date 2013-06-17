module Main where

import Language.SPL.Parser
import Language.SPL.Printer
import Language.SPL.Analyser

import Language.SPL.Program
import Language.SPL.Environment
import Language.SPL.Error

import Control.Monad.Writer
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.MultiMap as Multi

