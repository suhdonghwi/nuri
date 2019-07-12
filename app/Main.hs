module Main where

import           System.Environment

import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Repl

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then runRepl repl (ReplState ">> " Map.empty "(반응형)")
    else do
      let filePath = head args
      content <- readFile filePath
      runRepl (evalInput (T.pack content)) (ReplState ">> " Map.empty filePath)
