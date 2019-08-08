module Repl where

import           System.Exit
import           System.IO

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State

import           Control.Lens
import           Control.Lens.TH

import qualified Data.Text.IO                  as TextIO
import qualified Data.Text                     as T
import           Data.Map

import           Text.Megaparsec

import           Nuri.Eval.Stmt
import qualified Nuri.Eval.Val                 as V
import           Nuri.Parse.Stmt

data ReplState = ReplState { _prompt :: T.Text, _symbolTable :: V.SymbolTable, _fileName :: String }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

intrinsicTable :: V.SymbolTable
intrinsicTable = fromList []

evalInput :: T.Text -> Repl (Maybe (V.Flow V.Val))
evalInput input = do
  st <- get
  let ast = runParser (parseStmt <* eof) (view fileName st) input
  case ast of
    Left  err    -> liftIO $ putStrLn (errorBundlePretty err) >> return Nothing
    Right result -> do
      evalResult <- liftIO
        $ runStmtEval result (V.InterpreterState (view symbolTable st) False)
      case evalResult of
        Left evalErr -> liftIO $ putStrLn (show evalErr) >> return Nothing
        Right (finalValue, st') -> do
          modify $ set symbolTable (view V.symbolTable st')
          return $ Just finalValue

repl :: Repl ()
repl = do
  st <- get
  liftIO $ do
    TextIO.putStr (view prompt st)
    hFlush stdout
  line <- T.strip <$> liftIO TextIO.getLine
  liftIO $ when (line == ":quit") exitSuccess
  result <- evalInput line
  case result of
    Just val -> liftIO $ print val
    Nothing  -> return ()
  repl

runRepl :: Repl a -> ReplState -> IO ()
runRepl f st = runStateT (unRepl f) st >> return ()
