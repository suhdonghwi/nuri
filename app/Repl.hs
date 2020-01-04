module Repl where

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
import           Haneul.Instruction
import           Haneul.Pretty                            ( )
import           Haneul.Serial                            ( )

data ReplState = ReplState { _prompt :: Text }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

parseInput :: Text -> Text -> MaybeT IO Stmts
parseInput input fileName = do
  case runParser (parseStmts <* eof) (toString fileName) input of
    Left err -> do
      (liftIO . putTextLn . toText . errorBundlePretty) err
      hoistMaybe Nothing
    Right parseResult -> return parseResult

printResult :: Stmts -> IO ()
printResult val = do
  (liftIO . print . vsep . toList) (pretty <$> val)
  let (internal, insts) =
        execRWS (sequence $ compileStmt <$> val) "(반응형)" defaultInternal
  putStrLn "---------------"
  print $ pretty internal
  (print . vsep) (pretty <$> insts)
  putStrLn "---------------"
  let encodedInternal = encode internal
      encodedInsts    = encode insts

  putStrLn $ concat $ ("\\x" ++) . printf "%02x" <$> unpackBytes
    (encodedInternal <> encodedInsts)

  when ((decode encodedInternal :: BuilderInternal) == internal)
       (putStrLn "Internal valid")
  when ((decode encodedInsts :: Code) == insts) (putStrLn "Insts valid")

repl :: Repl ()
repl = forever $ do
  st <- get
  liftIO $ do
    putText (view prompt st)
    hFlush stdout
  input <- strip <$> liftIO getLine
  liftIO $ when (input == ":quit") exitSuccess
  result <- (liftIO . runMaybeT . parseInput input) "(반응형)"
  case result of
    Just stmts -> liftIO (printResult stmts)
    Nothing    -> pass

runRepl :: Repl a -> ReplState -> IO a
runRepl f = evalStateT (unRepl f)
