{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad (when)
import Data.Maybe (fromJust)
import Helper (compileResult)
import Nuri.Parse.Stmt (parseInput)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import System.Process (callCommand)
import System.Console.CmdArgs
import System.Console.CmdArgs.Explicit
import qualified Data.Text.IO.Utf8 as Utf8
import Main.Utf8 (withUtf8)

defaultHaneulPath :: FilePath
#ifdef mingw32_HOST_OS
defaultHaneulPath = ".\\haneul.exe"
#else
defaultHaneulPath = "./haneul"
#endif

data Nuri = Run { src :: FilePath, haneul :: FilePath, debug :: Bool }
  deriving (Show, Data, Typeable)

run = Run { 
            src = def &= args &= typ "누리 코드 파일",
            haneul = defaultHaneulPath &= typ "[하늘 파일]" &= groupname "옵션",
            debug = def
          }
      &= summary "누리 0.2.1 - 함수형 한글 프로그래밍 언어"
      &= helpArg [help "도움 메시지를 출력합니다."]
      &= versionArg [help "누리의 버전을 출력합니다."]

main :: IO ()
main = withUtf8 $ do
  let mode = cmdArgsMode run
  let helpMessage = helpText [] HelpFormatDefault mode
  Run {src = inputPath, haneul = haneulPath, debug = isDebug} <- cmdArgs run
  when (null inputPath) $ do
    print helpMessage
    exitFailure

  inputExists <- doesFileExist inputPath
  when (not inputExists) $ do
    putStrLn $ "오류 : 입력 코드 파일 '" ++ inputPath ++ "' 파일을 찾을 수 없습니다."
    exitFailure

  content <- Utf8.readFile inputPath
  (result, _) <- parseInput content inputPath

  let bytecodeFileName = replaceExtension inputPath ".hn"
  compileResult inputPath isDebug bytecodeFileName result

  haneulExists <- doesFileExist haneulPath
  when (not haneulExists) $ do
    putStrLn $ "오류 : 하늘 실행 파일 '" ++ haneulPath ++ "' 파일을 찾을 수 없습니다. (--haneul 옵션을 사용하여 경로를 설정하세요.)"
    exitFailure
  callCommand $ haneulPath ++ " \"" ++ bytecodeFileName ++ "\""
