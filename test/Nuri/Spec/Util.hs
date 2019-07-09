module Nuri.Spec.Util where

import           Data.Text

import           Text.Megaparsec.Pos

import           Nuri.Expr
import           Nuri.Stmt

initPos :: SourcePos
initPos = initialPos "(test)"

newPos :: Int -> Int -> SourcePos
newPos p1 p2 = SourcePos "(test)" (mkPos p1) (mkPos p2)

litInteger :: Integer -> Expr
litInteger i = Lit initPos (LitInteger i)

binaryOp :: Op -> Expr -> Expr -> Expr
binaryOp = BinaryOp initPos

unaryOp :: Op -> Expr -> Expr
unaryOp = UnaryOp initPos

var :: Text -> Expr
var = Var initPos

app :: Expr -> [Expr] -> Expr
app = App initPos

funcDecl :: Text -> [Text] -> [Stmt] -> Stmt
funcDecl = FuncDecl initPos
