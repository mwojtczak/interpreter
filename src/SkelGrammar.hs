module SkelGrammar where

-- Haskell module generated by the BNF converter

import AbsGrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Program topdefs -> failure x
transTopDef :: TopDef -> Result
transTopDef x = case x of
  FnDef ident args block -> failure x
transArg :: Arg -> Result
transArg x = case x of
  CallVal ident -> failure x
  CallVar ident -> failure x
  FunArg ident -> failure x
transParam :: Param -> Result
transParam x = case x of
  DeclParam expr -> failure x
  AnonParam args block -> failure x
transLoc :: Loc -> Result
transLoc x = case x of
  DLocal ident -> failure x
transBlock :: Block -> Result
transBlock x = case x of
  Block stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  Empty -> failure x
  BStmt locs block -> failure x
  Ass ident expr -> failure x
  FAss ident args block -> failure x
  AddExpr ident expr -> failure x
  SubExpr ident expr -> failure x
  MulExpr ident expr -> failure x
  DivExpr ident expr -> failure x
  Ret expr -> failure x
  VRet -> failure x
  SPrint expr -> failure x
  FReturn args block -> failure x
  CondIf expr stmt -> failure x
  CondElse expr stmt1 stmt2 -> failure x
  While expr stmt -> failure x
  For ident expr1 forkind expr2 stmt -> failure x
  SExp expr -> failure x
transExpr :: Expr -> Result
transExpr x = case x of
  EVar ident -> failure x
  ELitInt integer -> failure x
  ELitTrue -> failure x
  ELitFalse -> failure x
  EIncr ident -> failure x
  EDecr ident -> failure x
  EPreIncr ident -> failure x
  EPreDecr ident -> failure x
  EApp ident params -> failure x
  Neg expr -> failure x
  Not expr -> failure x
  EMul expr1 mulop expr2 -> failure x
  EAdd expr1 addop expr2 -> failure x
  ERel expr1 relop expr2 -> failure x
  EAnd expr1 expr2 -> failure x
  EOr expr1 expr2 -> failure x
transForKind :: ForKind -> Result
transForKind x = case x of
  To -> failure x
  Downto -> failure x
transAddOp :: AddOp -> Result
transAddOp x = case x of
  Plus -> failure x
  Minus -> failure x
transMulOp :: MulOp -> Result
transMulOp x = case x of
  Times -> failure x
  Div -> failure x
  Mod -> failure x
transRelOp :: RelOp -> Result
transRelOp x = case x of
  LTH -> failure x
  LE -> failure x
  GTH -> failure x
  GE -> failure x
  EQU -> failure x
  NE -> failure x

