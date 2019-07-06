module Nuri.Spec.Parse.Util where

import           Data.Text

import           Text.Megaparsec

import           Nuri.Expr
import           Nuri.Stmt

import           Nuri.Spec.Util

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

testParse :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
testParse parser = parse parser "(test)"
