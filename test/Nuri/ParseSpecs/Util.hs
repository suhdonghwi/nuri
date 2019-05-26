module Nuri.ParseSpecs.Util where

import           Text.Megaparsec
import           Text.Megaparsec.Error

import           Nuri.Expr
import           Nuri.Stmt

p = initialPos "(test)"
litInteger i = Lit p (LitInteger i)
binaryOp = BinaryOp p
unaryOp = UnaryOp p
var = Var p
app = App p

funcDecl = FuncDecl p

testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse parser input = parse parser "(test)" input
