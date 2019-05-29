module Nuri.Util where

import           Data.Text

import           Text.Megaparsec

import           Nuri.Expr
import           Nuri.Stmt

p :: SourcePos
p = initialPos "(test)"

litInteger :: Integer -> Expr
litInteger i = Lit p (LitInteger i)

binaryOp :: Op -> Expr -> Expr -> Expr
binaryOp = BinaryOp p

unaryOp :: Op -> Expr -> Expr
unaryOp = UnaryOp p

var :: Text -> Expr
var = Var p

app :: Text -> [Expr] -> Expr
app = App p

funcDecl :: Text -> [Text] -> [Stmt] -> Stmt
funcDecl = FuncDecl p

testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse parser = parse parser "(test)"
