module Repl where

import           Prelude                           hiding ( writeFile )

import           System.IO                                ( hFlush )

import           Data.Text                                ( strip )
import           Data.ByteString.Lazy.Internal            ( unpackBytes )

import           Control.Monad.RWS                        ( execRWS )
import           Control.Lens
import           Control.Lens.TH                          ( )

import           Data.Text.Prettyprint.Doc                ( pretty
                                                          , vsep
                                                          )
import           Data.Binary                              ( encode
                                                          , decode
                                                          )

import           Text.Megaparsec
import           Text.Printf

import           Nuri.Stmt
import           Nuri.Pretty                              ( )
import           Nuri.Parse.Stmt
import           Nuri.Codegen.Stmt

import           Haneul.Builder
import           Haneul.Pretty                            ( )
import           Haneul.Serial                            ( )

newtype ReplState = ReplState { _prompt :: Text }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

parseInput :: String -> String -> MaybeT IO Stmts
parseInput input fileName = do
  case runParser (parseStmts <* eof) fileName input of
    Left err -> do
      (liftIO . putTextLn . toText . errorBundlePretty) err
      hoistMaybe Nothing
    Right parseResult -> return parseResult

compileStmts :: Stmts -> Program
compileStmts stmts = uncurry Program
  $ execRWS (sequence_ $ compileStmt <$> stmts) 0 defaultInternal

printResult :: Stmts -> IO ()
printResult stmts = do
  (liftIO . print . vsep . toList) (pretty <$> stmts)
  let program          = compileStmts stmts
      compiledCode     = view programCode program
      compiledInternal = view programInternal program

  putStrLn "---------------"
  print $ pretty compiledInternal
  (print . vsep) (pretty <$> compiledCode)
  putStrLn "---------------"

  let encodedProgram = encode program
  writeFileLBS "./test.hn" encodedProgram
  putStrLn $ concat $ ("\\x" ++) . printf "%02x" <$> unpackBytes encodedProgram
  when ((decode encodedProgram :: Program) == program)
       (putStrLn "Program encoding is valid")

repl :: Repl ()
repl = forever $ do
  st <- get
  liftIO $ do
    putText (view prompt st)
    hFlush stdout
  input <- toString . strip <$> liftIO getLine
  liftIO $ when (input == ":quit") exitSuccess
  result <- (liftIO . runMaybeT . parseInput input) "(반응형)"
  case result of
    Just stmts -> liftIO (printResult stmts)
    Nothing    -> pass

runRepl :: Repl a -> ReplState -> IO a
runRepl f = evalStateT (unRepl f)
