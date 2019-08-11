module Nuri.Spec.Util where

import           Text.Megaparsec.Pos

import           Nuri.Expr
import           Nuri.Stmt

initPos :: SourcePos
initPos = initialPos "(test)"

litInteger :: Integer -> Expr
litInteger v = Lit initPos (LitInteger v)

litReal :: Double -> Expr
litReal v = Lit initPos (LitReal v)

litBool :: Bool -> Expr
litBool v = Lit initPos (LitBool v)

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

ifStmt :: Expr -> Stmt -> Maybe Stmt -> Stmt
ifStmt = If initPos

funcDecl :: Text -> [Text] -> Stmt -> Stmt
funcDecl = FuncDecl initPos
