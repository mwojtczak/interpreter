{-# LANGUAGE ScopedTypeVariables #-}

module MilMonad where

import LexGrammar
import ParGrammar
import AbsGrammar
import ErrM
import Control.Monad.State
import Control.Monad.Error
import Data.Maybe
import qualified Data.Map as Map


type Environment = Map.Map String Integer
type Store = Map.Map Integer Value
type MIL = ErrorT String (StateT (Environment, Store, Integer) IO)
data Value = VBool Bool | VInteger Integer | NoReturn |Function (Ident, [Arg], Block, Environment)

instance Show Value where
	show (VInteger x) = show x
	show (VBool True) = "true"
	show (VBool False) = "false"
	show (Function ((Ident name), args, body, store)) = "function " ++ name ++ " - " ++  (show args)
	show (NoReturn) = "no return"

instance Eq Value where
	(VInteger x) == (VInteger y) = x == y
	(VBool x) == (VBool y) = x == y
	(VBool x) == (NoReturn) = False
	(NoReturn) == (NoReturn) = True
	(_) == (NoReturn) = False
	(VInteger x) /= (VInteger y) = x /= y
	(VBool x) /= (VBool y) = x /= y


getStore :: MIL Store
getStore = do
	(_, store, _)<- get
	return store 

getEnvironment :: MIL Environment
getEnvironment = do
	(env, _, _) <- get
	return env 

getAllocCounter :: MIL Integer
getAllocCounter = do
	(env, store, counter) <- get
	return counter

setAllocCounter :: MIL Integer
setAllocCounter = do
	(env, store, counter) <- get
	put (env, store, counter + 1)
	return counter

setStore :: Store -> MIL ()
setStore store = do
	(env, _, counter) <- get
	put (env, store, counter)

setEnvironment :: Environment -> MIL ()
setEnvironment env = do
	(_, store, counter) <- get
	put (env, store, counter)

getValue :: String -> MIL Value
getValue var = do
	(env, store, _) <- get
	if (Map.member var env) then
		return $ store Map.! (env Map.! var)
	else
		throwError $ var ++ " is undeclared"

getSaveValue :: String -> MIL (Maybe Value)
getSaveValue var = do
	(env, store, _) <- get
	let location = Map.lookup var env
	case location of 
		Nothing -> return Nothing
		(Just loc) -> return $ Map.lookup loc store

setLocation :: String -> MIL Integer
setLocation var = do
	env <- getEnvironment
	loc <- setAllocCounter
	let env' = Map.insert var (toInteger $ loc) env
	setEnvironment env'
	return $ toInteger $ loc

getLocation :: String -> MIL Integer
getLocation var = do
	env <- getEnvironment
	return $ env Map.! var

setValue :: Integer -> Value -> MIL ()
setValue loc val = do
	store <- getStore
	setStore $ Map.insert loc val store

updateVariable :: String -> Value -> MIL ()
updateVariable var val = do
	env <- getEnvironment
	loc <- if (Map.member var env) then
		getLocation var
	else
		setLocation var
	setValue loc val


