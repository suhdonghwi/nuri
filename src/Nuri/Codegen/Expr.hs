module Nuri.Codegen.Expr where

import Control.Lens
  ( assign,
    modifying,
    use,
    uses,
    view,
  )
import Control.Monad.RWS (execRWS)
import qualified Data.List as L
import qualified Data.Set.Ordered as S
import Haneul.Builder
  ( Builder,
    addConstant,
    addFreeVar,
    addGlobalVarName,
    addVarName,
    createMark,
    internalToFuncObject,
    setMark,
    tellCode,
    tellInst,
  )
import Haneul.BuilderInternal
  ( BuilderInternal,
    defaultInternal,
    internalDepth,
    internalFreeVars,
    internalLocalVars,
    internalMaxLocalCount,
    _internalLastFilePath,
    _internalLastLine,
  )
import Haneul.Constant (Constant (..), FuncObject (_funcJosa), _funcFilePath, _funcLineNo, _funcName)
import Haneul.Instruction (Mark (Mark))
import qualified Haneul.Instruction as Inst
  ( Instruction' (..),
  )
import Nuri.ASTNode (ASTNode (getSourcePos))
import Nuri.Expr
  ( BinaryOperator (..),
    Decl (Decl),
    DeclType (..),
    Expr (..),
    UnaryOperator (LogicNot, Negative, Positive),
  )
import Nuri.Literal (Literal (..))
import Text.Megaparsec.Pos (SourcePos, sourceLine, sourceName)

litToConst :: Literal -> Constant
litToConst LitNone = ConstNone
litToConst (LitInteger v) = ConstInteger v
litToConst (LitReal v) = ConstReal v
litToConst (LitChar v) = ConstChar v
litToConst (LitBool v) = ConstBool v

compileExpr :: Expr -> Builder ()
compileExpr (Lit pos lit) = do
  let value = litToConst lit
  index <- addConstant value
  tellInst pos (Inst.Push $ fromIntegral index)
compileExpr (Var pos ident) = do
  varNames <- use internalLocalVars
  let identIndices = L.elemIndices ident (snd <$> toList varNames)
  case viaNonEmpty last identIndices of
    Just index -> tellInst pos (Inst.LoadLocal $ fromIntegral index)
    Nothing -> do
      -- localVars 중에서 식별자를 찾지 못했을 경우
      outerVars <- ask
      let result =
            -- 외부 범위에서 변수 식별자 찾기
            viaNonEmpty
              head
              [ (i, j)
                | (i, scope) <- zip [0 ..] outerVars,
                  (j, x) <- zip [0 ..] $ toList scope,
                  x == ident
              ]
      case result of
        Just loc -> do
          index <- addFreeVar loc
          tellInst pos (Inst.LoadDeref index)
        Nothing -> do
          index <- addGlobalVarName ident
          tellInst pos (Inst.LoadGlobal index)
compileExpr (FuncCall pos func args) = do
  sequence_ (compileExpr . fst <$> args)
  compileExpr func
  tellInst pos (Inst.Call $ snd <$> reverse args)
compileExpr (If pos condExpr thenExpr elseExpr) = do
  compileExpr condExpr
  whenFalseMark <- createMark
  tellInst pos (Inst.PopJmpIfFalse $ Mark whenFalseMark)

  compileExpr thenExpr
  thenJumpMark <- createMark
  tellInst pos (Inst.Jmp $ Mark thenJumpMark)

  setMark whenFalseMark
  compileExpr elseExpr
  setMark thenJumpMark
compileExpr (BinaryOp pos op lhs rhs) = do
  compileExpr lhs
  compileExpr rhs
  case op of
    Add -> tellInst pos (Inst.Add)
    Subtract -> tellInst pos (Inst.Subtract)
    Multiply -> tellInst pos (Inst.Multiply)
    Divide -> tellInst pos (Inst.Divide)
    Mod -> tellInst pos (Inst.Mod)
    Equal -> tellInst pos (Inst.Equal)
    Inequal -> tellCode pos [Inst.Equal, Inst.LogicNot]
    LessThan -> tellInst pos (Inst.LessThan)
    GreaterThan -> tellInst pos (Inst.GreaterThan)
    LessThanEqual -> tellCode pos [Inst.GreaterThan, Inst.LogicNot]
    GreaterThanEqual -> tellCode pos [Inst.LessThan, Inst.LogicNot]
    LogicAnd -> tellInst pos Inst.LogicAnd
    LogicOr -> tellInst pos Inst.LogicOr
compileExpr (UnaryOp pos op value) = do
  compileExpr value
  case op of
    Positive -> pass
    Negative -> tellInst pos Inst.Negate
    LogicNot -> tellInst pos Inst.LogicNot
compileExpr (Seq xs) = do
  tempVarNames <- use internalLocalVars

  modifying internalDepth (+ 1)
  depth <- use internalDepth

  let process [] = pass
      process (y : ys) = do
        case y of
          Left (Decl pos name t) -> do
            let declList = declToExpr pos name t
            -- 여기에는 구조체 선언문이 올 수 없음
            sequence_ (addVarName depth . fst <$> declList)

            let register (n, b) = 
                  case b of
                    Just build -> do
                      build
                      index <- addVarName depth n
                      tellInst pos (Inst.StoreLocal index)
                    Nothing -> pass

            sequence_ (register <$> declList)
          Right expr -> do
            compileExpr expr
            -- 시퀀스의 마지막이 아닌 표현식일 경우 Pop
            when (not $ null ys) (tellInst (getSourcePos expr) Inst.Pop)
        process ys
  process $ toList xs

  maxLocalCount <- use internalMaxLocalCount
  localCount <- uses internalLocalVars (fromIntegral . S.size)
  when (localCount > maxLocalCount) (assign internalMaxLocalCount localCount)

  assign internalLocalVars tempVarNames

  modifying internalDepth (`subtract` 1)
compileExpr (Lambda pos name args body) = do
  (internal, funcObject) <- lambdaToFuncObject pos name args (compileExpr body)

  index <- addConstant (ConstFunc funcObject)
  tellInst pos (Inst.Push index)

  let freeVarList = toList $ view internalFreeVars internal
      processFreeVar [] = return []
      processFreeVar ((depth, localIndex) : xs) = do
        val <-
          if depth == 0
            then return (False, localIndex) -- 자유 변수가 현재 로컬에 있을 경우
            else do
              -- 자유 변수가 상위 스코프에 있을 경우
              freeIndex <- addFreeVar (depth - 1, localIndex)
              return (True, fromIntegral freeIndex)
        fmap (val :) (processFreeVar xs)

  processed <- processFreeVar freeVarList
  when ((not . null) processed) $ tellInst pos (Inst.FreeVar processed)
compileExpr (Struct pos name fields) =
  do
    let fieldNames = fst <$> fields
        values = snd <$> fields

    sequence_ (compileExpr <$> values)
    tellInst pos (Inst.MakeStruct name (reverse fieldNames))

lambdaToFuncObject ::
  SourcePos -> Text -> [(Text, Text)] -> Builder () -> Builder (BuilderInternal, FuncObject)
lambdaToFuncObject pos name args body = do
  localVars <- use internalLocalVars
  oldLocalStack <- ask
  let newLocalStack = (S.fromList . fmap snd . toList) localVars
      (internal, code) =
        execRWS
          ( do
              sequence_ (addVarName 0 . fst <$> args)
              body
          )
          (newLocalStack : oldLocalStack)
          (defaultInternal {_internalLastLine = sourceLine pos, _internalLastFilePath = sourceName pos})
      result = internalToFuncObject (internal, code)
  return
    ( internal,
      result
        { _funcJosa = snd <$> args,
          _funcLineNo = sourceLine pos,
          _funcName = name,
          _funcFilePath = sourceName pos
        }
    )

declToExpr :: SourcePos -> Text -> DeclType -> [(Text, Maybe (Builder ()))]
declToExpr pos name t =
  case t of
    FuncDecl _ args body -> 
      case body of
        Just body' -> [(name, Just $ compileExpr $ Lambda pos name args body')]
        Nothing -> [(name, Nothing)]
    ConstDecl expr -> [(name, Just $ compileExpr expr)]
    StructDecl fields ->
      let fieldGetter field =
            ( field,
              Just $ do
                let fieldGetterBody = do
                      compileExpr (Var pos "구조체")
                      tellInst pos (Inst.GetField field)
                (_, funcObject) <- lambdaToFuncObject pos field [("구조체", "의")] fieldGetterBody
                index <- addConstant (ConstFunc funcObject)
                tellInst pos (Inst.Push index)
            )
       in (fieldGetter <$> fields)
