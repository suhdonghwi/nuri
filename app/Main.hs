{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Repl
import System.Console.Docopt
import System.Environment (getArgs)

patterns :: Docopt
patterns = [docoptFile|USAGE.txt|]

main :: IO ()
main = do
  opts <- parseArgsOrExit patterns =<< getArgs

  when (opts `isPresent` (longOption "help")) $ do
    let helpMessage =
          [ "누리 - 함수형 한글 프로그래밍 언어",
            "",
            "사용법:",
            "  nuri --help | -h",
            "  nuri --version | -v",
            "  nuri <파일명>",
            "",
            "옵션:",
            "  -h, --help      도움말을 출력합니다.",
            "  -v, --version   누리 실행기의 버전을 출력합니다."
          ] ::
            [Text]
    putTextLn $ unlines helpMessage
    exitSuccess
  when (opts `isPresent` (longOption "version")) $ do
    putStrLn "누리 - 배포 전 0.1"
    exitSuccess
  when (opts `isPresent` (argument "file")) $ do
    let filePath = fromJust $ opts `getArg` (argument "file")
    content <- readFileText filePath
    result <- runMaybeT $ parseInput content filePath
    whenJust result printResult
