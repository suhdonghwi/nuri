{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Nuri.Spec.Util where

import Nuri.Expr
import Nuri.Literal
import Nuri.Stmt
import Text.Megaparsec.Pos

initPos :: SourcePos
initPos = initialPos "(test)"

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

lambda = Lambda initPos ""

struct = Struct initPos

funcDecl kind name args body = Decl initPos name $ FuncDecl kind args (Just body)

funcDeclStmt = (((DeclStmt .) .) .) . funcDecl

funcForward kind name args = Decl initPos name $ FuncDecl kind args Nothing

funcForwardStmt = ((DeclStmt .) .) . funcForward

constDecl name expr = Decl initPos name (ConstDecl expr)

constDeclStmt = (DeclStmt .) . constDecl

structDecl name fields = Decl initPos name (StructDecl fields)

structDeclStmt = (DeclStmt .) . structDecl
