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

decl = Decl initPos

funcDecl name args body = decl name $ FuncDecl args (Just body)

funcDeclStmt = ((DeclStmt .) .) . funcDecl

verbDecl name args body = decl name $ VerbDecl args (Just body)

verbDeclStmt = ((DeclStmt .) .) . verbDecl

adjectiveDecl name args vars body = decl name $ AdjectiveDecl args vars (Just body)

adjectiveDeclStmt = (((DeclStmt .) .) .) . adjectiveDecl

funcForward name args = decl name $ FuncDecl args Nothing

funcForwardStmt = (DeclStmt .) . funcForward

verbForward name args = decl name $ VerbDecl args Nothing

verbForwardStmt = (DeclStmt .) . verbForward

adjectiveForward name args vars = decl name $ AdjectiveDecl args vars Nothing

adjectiveForwardStmt = ((DeclStmt .) .) .  adjectiveForward

constDecl name expr = decl name (ConstDecl expr)

constDeclStmt = (DeclStmt .) . constDecl

structDecl name fields = decl name (StructDecl fields)

structDeclStmt = (DeclStmt .) . structDecl
