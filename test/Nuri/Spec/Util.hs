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
litInteger v = Lit initPos (LitInteger v)

litReal :: Double -> Expr
litReal v = Lit initPos (LitReal v)

binaryOp :: Op -> Expr -> Expr -> Expr
binaryOp = BinaryOp initPos

unaryOp :: Op -> Expr -> Expr
unaryOp = UnaryOp initPos

var :: Text -> Expr
var = Var initPos

app :: Expr -> [Expr] -> Expr
app = App initPos

assign :: Text -> Expr -> Expr
assign = Assign initPos

funcDecl :: Text -> [Text] -> [Stmt] -> Stmt
funcDecl = FuncDecl initPos
