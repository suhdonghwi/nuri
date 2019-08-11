module Repl where

import           System.IO                                ( hFlush )

import           Control.Lens
import           Control.Lens.TH

import qualified Data.Map                      as M
import           Data.Text                                ( strip )

import           Text.Megaparsec

import           Nuri.Eval.Stmt
import qualified Nuri.Eval.Val                 as V
import           Nuri.Parse.Stmt

data ReplState = ReplState { _prompt :: Text, _symbolTable :: V.SymbolTable, _fileName :: Text }

$(makeLenses ''ReplState)

newtype Repl a = Repl { unRepl :: StateT ReplState IO a }
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

intrinsicTable :: V.SymbolTable
intrinsicTable = M.fromList []

evalInput :: Text -> Repl (Maybe (V.Flow V.Val))
evalInput input = do
  st <- get
  let ast = runParser (parseStmt <* eof) (toString $ view fileName st) input
  case ast of
    Left  err    -> liftIO $ putStrLn (errorBundlePretty err) >> return Nothing
    Right result -> do
      evalResult <- liftIO
        $ runStmtEval result (V.InterpreterState (view symbolTable st) False)
      case evalResult of
        Left  evalErr           -> liftIO $ print evalErr >> return Nothing
        Right (finalValue, st') -> do
          modify $ set symbolTable (view V.symbolTable st')
          return $ Just finalValue

repl :: Repl ()
repl = do
  st <- get
  liftIO $ do
    putText (view prompt st)
    hFlush stdout
  line <- strip <$> liftIO getLine
  liftIO $ when (line == ":quit") exitSuccess
  result <- evalInput line
  case result of
    Just val -> liftIO $ print val
    Nothing  -> pass
  repl

runRepl :: Repl a -> ReplState -> IO ()
runRepl f st = void $ runStateT (unRepl f) st
