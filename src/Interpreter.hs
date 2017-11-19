{-# LANGUAGE ScopedTypeVariables #-}

import System.Environment hiding (getEnvironment)

import LexGrammar
import ParGrammar
import AbsGrammar
import ErrM
import Control.Monad.State
import Control.Monad.Error
import qualified Data.Map as Map
import Data.Map.Lazy as M
import Data.Maybe

import MilMonad



--gets all functions from file
--execs function called "main"
collectFunctions :: [TopDef] -> MIL ()
collectFunctions [] = do
	execMainFunction
	return ()

collectFunctions ((FnDef (Ident name) args block):xs) = do
	env <- getEnvironment
	updateVariable name (Function ((Ident name), args, block, env))
	(enva,  storea, countera) <- get
	collectFunctions xs


--adds variable name to env with new location, adds it also to store
updateParamValue :: String -> Value -> Environment -> Store -> MIL(Environment, Store, Integer)
updateParamValue name value env store = do
	loc <- setAllocCounter
	let env' = Map.insert name loc env
	let store' = Map.insert loc value store
	allocCounter <- getAllocCounter
	return $ (env', store', allocCounter)


--adds parameters to evn and store depending on its type and way of its passing (var or val)
updateArgs :: [Arg] -> [Param] -> Environment -> Store -> Integer -> MIL (Environment, Store, Integer)
updateArgs [] [] env store count = do
	(env', store', counter) <-  updateParamValue ("__return__") (VInteger 0) env store
	return $ (env', store', counter)

updateArgs ((CallVal (Ident name)):xs) ((DeclParam expr):ys) env store count = do
	value <- evalExp expr
	(env', store', counter) <- updateParamValue name value env store
	updateArgs xs ys env' store' counter

updateArgs ((CallVar (Ident name)):xs) ((DeclParam (EVar (Ident param_name))):ys) env store count = do
	location <- getLocation param_name
	let env' = Map.insert name location env
	updateArgs xs ys env' store count

updateArgs ((FunArg (Ident name)):xs) ((DeclParam (EVar (Ident fun_name))):ys) env store count = do
	fun_def <- getValue fun_name
	(env', store', counter) <- updateParamValue name fun_def env store
	updateArgs xs ys env' store' counter

updateArgs ((FunArg (Ident name)):xs) ((AnonParam args block):ys) env store count = do
	(env', store', counter) <- updateParamValue name (Function ((Ident name), args, block, env)) env store
	updateArgs xs ys env' store' counter


execFunction :: Ident -> [Param] -> MIL Value
execFunction (Ident name) params = do
	(curr_env, curr_store, curr_counter) <- get
	ggg <- getValue name
	(Function (_, args, (Block body), decl_env)) <- getValue name
	if length args /= length params then
		if length args > length params then
			throwError $ "Not enough parameters in " ++ name ++ " function invocation"
		else
			throwError $ "Too many parameters in " ++ name ++ " function invocation"
	else do
		(rec_env, rec_store, counter1) <- updateParamValue name (Function ((Ident name), args, (Block body), decl_env)) decl_env curr_store
		(env, store, counter) <- updateArgs args params rec_env rec_store counter1
		(_, _, new_counter) <- get
		put (env, store, new_counter)
		execStmt body
		n <- getSaveValue "__return__"
		let val = case (n) of
			(Nothing) -> (VInteger 0)
			(Just NoReturn) -> (VInteger 0)
			(Just x) -> x
		setEnvironment curr_env
		return val


execMainFunction :: MIL ()
execMainFunction = do
	n <- execFunction (Ident "main") []
	return ()


updateEnvWithLocals :: [Loc] -> Environment -> Store -> MIL(Environment, Store, Integer)
updateEnvWithLocals [] env store = do
	counter <- getAllocCounter
	return (env, store, counter)
updateEnvWithLocals ((DLocal (Ident name)): xs) env store = do
	(env', store', counter) <- updateParamValue name (VInteger 0) env store
	updateEnvWithLocals xs env' store'


execStmt :: [Stmt] -> MIL ()
execStmt [] = return ()
execStmt ((SExp expr):xs) = do
	_ <- evalExp expr
	execStmt xs

execStmt ((Ass (Ident var) e):xs) = do
	n <- evalExp e
	updateVariable var n
	execStmt xs

execStmt ((FAss (Ident var) args block):xs) = do
	env <- getEnvironment
	updateVariable var  (Function ((Ident ""), args, block, env))
	execStmt xs

execStmt (VRet:xs) = do
	updateVariable "__return__" (VInteger 0)


execStmt ((Ret expr):xs) = do
	n <- evalExp expr
	updateVariable "__return__" n


execStmt ((SPrint expr):xs) = do
	value <- evalExp expr
	liftIO $ print $ value
	execStmt xs

execStmt ((FReturn args block):xs) = do
	env <- getEnvironment
	updateVariable "__return__" (Function ((Ident ""), args, block, env))

execStmt (Empty:xs) = execStmt xs

execStmt ((BStmt locals (Block stmts)):xs) = do
	(env, store, counter) <- get
	(evn', store', counter') <- updateEnvWithLocals locals env store
	put (evn', store', counter')
	execStmt stmts
	setEnvironment env
	execStmt xs

execStmt ((AddExpr (Ident var) expr):xs) = do
	(VInteger n) <- getValue var
	(VInteger val) <- evalExp expr
	updateVariable var $ VInteger $ n + val
	execStmt xs

execStmt ((SubExpr (Ident var) expr):xs) = do
	(VInteger n) <- getValue var
	(VInteger val) <- evalExp expr
	updateVariable var $ VInteger $ n - val
	execStmt xs

execStmt ((MulExpr (Ident var) expr):xs) = do
	(VInteger n) <- getValue var
	(VInteger val) <- evalExp expr
	updateVariable var $ VInteger $ n * val
	execStmt xs

execStmt ((DivExpr (Ident var) expr):xs) = do
	(VInteger n) <- getValue var
	(VInteger val) <- evalExp expr
	if (val /= 0) then do
		updateVariable var $ VInteger $ n `div` val
		execStmt xs
	else
		throwError $ "Division by 0"

execStmt ((CondElse e stmt1 stmt2):xs) = do
	(VBool b) <- evalExp e
	case b of 
		True -> execStmt [stmt1]
		False -> execStmt [stmt2]
	execStmt xs

execStmt ((CondIf e stmt):xs) = do
	(VBool b) <- evalExp e
	case b of
		True -> execStmt [stmt]
		otherwise -> return ()
	execStmt xs

execStmt ((While e stmt):xs) = do
	(VBool b) <- evalExp e
	case b of
		True -> execStmt [stmt, (While e stmt)]
		otherwise -> return ()
	execStmt xs
	
execStmt ((For (Ident var) exp_start op exp_end block):xs) = do
	(VInteger val_start) <- evalExp exp_start
	(VInteger val_end) <- evalExp exp_end
	(env, store, _) <- get
	(env', store', counter) <- updateParamValue var  (VInteger val_start) env store
	put (env', store', counter)
	case op of 
		To -> execStmt $ replicate (fromIntegral (val_end - val_start + 1)) (BStmt [] (Block  ([block, (SExp (EIncr (Ident var)))])))
		Downto -> execStmt $ replicate (fromIntegral (val_start - val_end + 1)) (BStmt [] (Block ( [block, (SExp (EDecr (Ident var)))])))
	setEnvironment env
	execStmt xs


evalExp :: Expr -> MIL Value
evalExp (ELitInt n) = return $ VInteger n

evalExp (Neg expr) = do
	val <- evalExp expr
	case val of 
		(VInteger n) -> return $ VInteger $ (-1) * n
		otherwise -> throwError "Cannot negate not Integer (operator -)"

evalExp (EMul e1 op e2) = do
	v1 <- evalExp e1
	_ <- getIntegerOrThrowError v1 $ "Expected type Integer for operation " ++ (showOp op)
	v2 <- evalExp e2
	_ <- getIntegerOrThrowError v2 $ "Expected type Integer for operation " ++ (showOp op)
	let (VInteger n1) = v1
	let (VInteger n2) = v2
	case op of 
		Times -> return $ VInteger $ n1 * n2
		Div -> if (n2 /= 0) then do return $ VInteger $ n1 `div` n2
			else
				throwError $ "division by 0"
		Mod -> return $ VInteger $ n1 `mod` n2

evalExp (EAdd e1 op e2) = do
	v1 <- evalExp e1
	_ <- getIntegerOrThrowError v1 $ "Expected type Integer for operation " ++ (showOpAdd op)
	v2 <- evalExp e2
	_ <- getIntegerOrThrowError v2 $ "Expected type Integer for operation " ++ (showOpAdd op)
	let (VInteger n1) = v1
	let (VInteger n2) = v2
	case op of 
		Minus -> return $ VInteger $ n1 - n2
		Plus -> return $ VInteger $ n1 + n2

evalExp (ELitTrue) = return $ VBool True

evalExp (ELitFalse) = return $ VBool False

evalExp (EVar (Ident x)) = getValue x

evalExp (ERel e1 op e2) = do
	val1 <- evalExp e1
	val2 <- evalExp e2
	case (isBoolRelOp op) of 
		True -> do
			_ <- isTheSameTypeOrThrowError val1 val2 $ "Expected the same type of data for relation operation " ++ (showRelOperation op)
			case op of
				EQU -> return $ VBool $ val1 == val2
				NE -> return $ VBool $ val1 /= val2
		otherwise -> do
			_ <- getIntegerOrThrowError val1 $ "Expected type Integer for in relation operation " ++ (showRelOperation op)
			_ <- getIntegerOrThrowError val2 $ "Expected type Integer for relation operation " ++ (showRelOperation op)
			let (VInteger n1) = val1
			let (VInteger n2) = val2
			case op of 
				LTH -> return $ VBool $ n1 < n2
				LE -> return $ VBool $ n1 <= n2
				GTH -> return $ VBool $ n1 > n2
				GE -> return $ VBool $ n1 >= n2

evalExp (EIncr (Ident var)) = do
	(VInteger n) <- getValue var
	updateVariable var $ VInteger $ n + 1 
	return $ VInteger n

evalExp (EDecr (Ident var)) = do
	(VInteger n) <- getValue var
	updateVariable var $ VInteger $ n - 1 
	return $ VInteger n


evalExp (EPreIncr (Ident var)) = do
	(VInteger n) <- getValue var
	updateVariable var $ VInteger $ n + 1 
	return $ VInteger $ n + 1

evalExp (EPreDecr (Ident var)) = do
	(VInteger n) <- getValue var
	updateVariable var $ VInteger $ n - 1 
	return $ VInteger $ n - 1

evalExp (Not e) = do
	(VBool b) <- evalExp e
	return $ VBool $ not b

--lazy AND
evalExp (EAnd e1 e2) = do
	(VBool b1) <- evalExp e1
	if b1 then do
		(VBool b2) <- evalExp e2
		return $ VBool $ all id [b1, b2]
	else do
		return $ VBool b1
	
--lazy OR
evalExp (EOr e1 e2) = do
	(VBool b1) <- evalExp e1
	if b1 then do
		return $ VBool b1
	else do
		(VBool b2) <- evalExp e2
		return $ VBool b2

evalExp (EApp (Ident name) params) = do
	(env, store, counter) <- get
	n <- execFunction (Ident name) params
	return n


isBoolRelOp :: RelOp -> Bool
isBoolRelOp op = elem op [EQU, NE]


showRelOperation :: RelOp -> String
showRelOperation op = 
	case op of 
		LTH -> "<"
		LE -> "<="
		GTH -> ">"
		GE -> ">="
		EQU -> "=="
		NE -> "!="


getIntegerOrThrowError :: Value -> String -> MIL()
getIntegerOrThrowError (VInteger x) _ = return ()
getIntegerOrThrowError _  error = throwError error


isTheSameTypeOrThrowError :: Value -> Value -> String -> MIL()
isTheSameTypeOrThrowError (VBool x) (VBool y) _ = return ()
isTheSameTypeOrThrowError (VInteger x) (VInteger y) _ = return ()
isTheSameTypeOrThrowError _ _ error = throwError error


showOp :: MulOp -> String
showOp (Times) = "*"
showOp (Div) = "/"
showOp (Mod) = "mod"


showOpAdd :: AddOp -> String
showOpAdd (Plus) = "+"
showOpAdd (Minus) = "-"


showErrors :: Either String () -> IO ()
showErrors (Left e) =  putStrLn e
showErrors (Right _) = return ()


main :: IO ()
main = do
	[filename] <- getArgs
	source <- readFile filename
	let program = pProgram $ myLexer source
	case program of 
		Bad s -> putStrLn $ "Program has a " ++ s
		Ok (Program defs) -> do
			output <- evalStateT (runErrorT $ collectFunctions defs ) (Map.empty, Map.empty, 0)
			showErrors output

