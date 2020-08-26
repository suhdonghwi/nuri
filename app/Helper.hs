module Helper where

import Control.Lens (makeLenses)
import Control.Lens.TH ()
import Data.Binary (encode)
import qualified Data.Set.Ordered as S
import Default (defineDefaults)
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
import Text.Megaparsec
  ( eof,
    runParser,
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
  case runParser (parseStmts <* eof) fileName input of
    Left err -> do
      (liftIO . putTextLn . toText . errorBundlePretty) err
      hoistMaybe Nothing
    Right result -> return result

compileResult :: Bool -> String -> (NonEmpty Stmt) -> IO ()
compileResult isDebug dest stmts = do
  let program' =
        ( internalToFuncObject
            . runBuilder defaultInternal {_internalGlobalVarNames = S.fromList (snd <$> defaultDecls)}
        )
          $ do
            defineDefaults
            compileStmts stmts
      program = program' {_funcFilePath = sourceName (getSourcePos $ head stmts)}

  when isDebug $ do
    (liftIO . pPrint) stmts
    putStrLn "---------------"
    pPrint program

  writeFileLBS dest (encode program)
