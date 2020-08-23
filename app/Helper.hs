module Helper where

import Control.Lens
  ( makeLenses,
    view,
  )
import Control.Lens.TH ()
import Data.Binary (encode)
import qualified Data.Set.Ordered as S
import Data.Text (strip)
import Haneul.Builder (internalToFuncObject, runBuilder)
import Haneul.BuilderInternal
  ( BuilderInternal (_internalGlobalVarNames),
    defaultDecls,
    defaultInternal,
  )
import Haneul.Constant (FuncObject (_funcFilePath))
import Haneul.Serial ()
import Nuri.ASTNode (ASTNode (getSourcePos))
import Nuri.Codegen.Stmt (compileStmts)
import Nuri.Parse.Error (errorBundlePretty)
import Nuri.Parse.Stmt (parseStmts)
import Nuri.Stmt (Stmt)
import System.IO (hFlush)
import Text.Megaparsec
  ( eof,
    runParserT,
    sourceName,
  )
import Text.Pretty.Simple (pPrint)
import Prelude hiding (writeFile)

newtype ReplState = ReplState {_prompt :: Text}

$(makeLenses ''ReplState)

newtype Repl a = Repl {unRepl :: StateT ReplState IO a}
  deriving (Monad, Functor, Applicative, MonadState ReplState, MonadIO)

parseInput :: Text -> String -> MaybeT IO (NonEmpty Stmt)
parseInput input fileName = do
  case evalState (runParserT (parseStmts <* eof) fileName input) defaultDecls of
    Left err -> do
      (liftIO . putTextLn . toText . errorBundlePretty) err
      hoistMaybe Nothing
    Right result -> return result

compileResult :: Bool -> String -> (NonEmpty Stmt) -> IO ()
compileResult isDebug dest stmts = do
  let program' =
        ( internalToFuncObject
            . runBuilder
              defaultInternal
                { _internalGlobalVarNames = S.fromList (snd <$> defaultDecls)
                }
            . compileStmts
        )
          stmts
      program = program' {_funcFilePath = sourceName (getSourcePos $ head stmts)}

  when isDebug $ do
    (liftIO . pPrint) stmts
    putStrLn "---------------"
    pPrint program

  writeFileLBS dest (encode program)
