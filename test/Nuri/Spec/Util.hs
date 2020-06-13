{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nuri.Spec.Util where

import Nuri.Expr
import Nuri.Literal
import Nuri.Stmt
import Text.Megaparsec.Pos

initPos :: Pos
initPos = pos1

litNone = Lit initPos LitNone

litInteger v = Lit initPos (LitInteger v)

litChar v = Lit initPos (LitChar v)

litReal v = Lit initPos (LitReal v)
litBool v = Lit initPos (LitBool v)


ifExpr = If initPos

binaryOp = BinaryOp initPos
unaryOp = UnaryOp initPos


var = Var initPos

funcCall = FuncCall initPos
lambda = Lambda initPos


funcDecl kind name args body = Decl initPos kind name (FuncDecl args body)

funcDeclStmt = (((DeclStmt .) .) .) . funcDecl

constDecl name expr = Decl initPos NormalDecl name (ConstDecl expr)

constDeclStmt = (DeclStmt .) . constDecl
