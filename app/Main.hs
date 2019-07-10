module Main where

import           Prelude
import           System.Environment

import qualified Data.Map                      as Map
import qualified Data.Text                     as T

import           Repl

main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then runRepl ">> " Map.empty
    else do
      let fileName = head args
      content <- readFile fileName
      evalInput (T.pack content) Map.empty fileName
