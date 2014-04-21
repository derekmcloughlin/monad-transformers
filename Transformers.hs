module Transformers where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map


type Name = String          -- Variable names

data Exp = Lit Integer      -- Expressions
         | Var Name
         | Plus Exp Exp
         | Abs Name Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer -- Values
           | FunVal Env Name Exp
           deriving (Show)


type Env = Map.Map Name Value

-- 1.1 Non-monadic Style

eval0 :: Env -> Exp -> Value
eval0 env (Lit i)      = IntVal i
eval0 env (Var n)       = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2)  = let IntVal i1 = eval0 env e1
                              IntVal i2 = eval0 env e2
                          in IntVal (i1 + i2)
eval0 env (Abs n e)     = FunVal env n e
eval0 env (App e1 e2)   = let val1 = eval0 env e1
                              val2 = eval0 env e2
                          in case val1 of
                                FunVal env' n body ->
                                    eval0 (Map.insert n val2 env') body

exampleExp = Lit 12 `Plus` App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2)

-- 2.1 Monadic Style

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 = runIdentity      -- Point free

eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i)       = return $ IntVal i
-- Note: For the Var case, in the original code we just have:
--      eval1 env (Var n)       = Map.lookup n env
-- However this causes a type error:
--      Couldn't match type `Maybe Value' with `Identity Value'
-- His explanation that 
--      Map.lookup is defined to work within any monad by simply calling the 
--      monad’s fail function
-- doesn't seem to work.
eval1 env (Var n)       = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2)  = do    
                            IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e)     = return $ FunVal env n e
eval1 env (App e1 e2)   = do
                            val1 <- eval1 env e1
                            val2 <- eval1 env e2
                            case val1 of
                                 FunVal env' n body ->
                                    eval1 (Map.insert n val2 env') body

-- 2.3 Adding Error Handling 

-- Attempt "a" - get it running with the ErrorT transformer

type Eval2 a = ErrorT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runErrorT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i)      = return $ IntVal i
eval2a env (Var n)      = return $ fromJust $ Map.lookup n env
eval2a env (Plus e1 e2) = do
                            IntVal i1 <- eval2a env e1
                            IntVal i2 <- eval2a env e2
                            return $ IntVal (i1 + i2)
eval2a env (Abs n e)    = return $ FunVal env n e
eval2a env (App e1 e2)  = do
                            val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                 FunVal env' n body ->
                                    eval2a (Map.insert n val2 env') body

-- Attempt "b" - Add useful error message

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i)      = return $ IntVal i
eval2b env (Var n)      = return $ fromJust $ Map.lookup n env
eval2b env (Plus e1 e2) = do
                            e1' <- eval2b env e1
                            e2' <- eval2b env e2
                            case (e1', e2') of 
                                (IntVal i1, IntVal i2) -> 
                                    return $ IntVal (i1 + i2)
                                _ ->
                                    throwError "type error in Plus"
eval2b env (Abs n e)    = return $ FunVal env n e
eval2b env (App e1 e2)  = do
                            val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                                FunVal env' n body ->
                                    eval2b (Map.insert n val2 env') body
                                _ -> throwError "type error"


-- This will fail with a type error because the "Abs" needs an argument
exampleExpFailPlus = Plus (Lit 1) (Abs "x" (Var "x"))

-- This will fail with a type error in "App"
exampleExpFailApp = App (Lit 1) (Lit 2)

-- Last attempt - add error handling to Map.lookup

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i)      = return $ IntVal i
eval2 env (Var n)      = case Map.lookup n env of
                            Just val -> return val
                            Nothing  -> throwError ("unbound variable " ++ n)
eval2 env (Plus e1 e2) = do
                            e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of 
                                (IntVal i1, IntVal i2) -> 
                                    return $ IntVal (i1 + i2)
                                _ ->
                                    throwError "type error in Plus"
eval2 env (Abs n e)    = return $ FunVal env n e
eval2 env (App e1 e2)  = do
                            val1 <- eval2 env e1
                            val2 <- eval2 env e2
                            case val1 of
                                FunVal env' n body ->
                                    eval2 (Map.insert n val2 env') body
                                _ -> throwError "type error in App"


exampleExpFailNoVar = Var "y"

-- 2.4 Hiding the Environment

type Eval3 a = ReaderT Env (ErrorT String Identity) a

runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity(runErrorT (runReaderT ev env))

eval3 :: Exp -> Eval3 Value
eval3 (Lit i)       = return $ IntVal i
eval3 (Var n)       = do
                        env <- ask
                        case Map.lookup n env of
                            Just val -> return val
                            Nothing  -> throwError ("unbound variable " ++ n)
eval3 (Plus e1 e2)  = do
                        e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of 
                            (IntVal i1, IntVal i2) -> 
                                return $ IntVal (i1 + i2)
                            _ ->
                                throwError "type error in Plus"
eval3 (Abs n e)     = do
                        env <- ask
                        return $ FunVal env n e
eval3 (App e1 e2)   = do
                        env <- ask
                        val1 <- eval3 e1
                        val2 <- eval3 e2
                        case val1 of
                            FunVal env' n body ->
                                local (const (Map.insert n val2 env')) (eval3 body)
                            _ -> throwError "type error in App"


-- 2.6 Adding State

type Eval4 a = ReaderT Env (ErrorT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity (runStateT (runErrorT (runReaderT ev env)) st)

-- The state is the number of steps (or ticks) in evaluating an expression
tick :: (Num s, MonadState s m) => m()
tick = do
        st <- get
        put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i)       = do
                        tick
                        return $ IntVal i
eval4 (Var n)       = do
                        tick
                        env <- ask
                        case Map.lookup n env of
                            Just val -> return val
                            Nothing  -> throwError ("unbound variable " ++ n)
eval4 (Plus e1 e2)  = do
                        tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of 
                            (IntVal i1, IntVal i2) -> 
                                return $ IntVal (i1 + i2)
                            _ ->
                                throwError "type error in Plus"
eval4 (Abs n e)     = do
                        tick
                        env <- ask
                        return $ FunVal env n e
eval4 (App e1 e2)   = do
                        tick
                        env <- ask
                        val1 <- eval4 e1
                        val2 <- eval4 e2
                        case val1 of
                            FunVal env' n body ->
                                local (const (Map.insert n val2 env')) (eval4 body)
                            _ -> throwError "type error in App"


-- Trying it with StateT and ErrorT swapped

type Eval4' a = ReaderT Env (StateT Integer (ErrorT String Identity)) a

runEval4' :: Env -> Integer -> Eval4' a -> (Either String (a, Integer))
runEval4' env st ev = runIdentity (runErrorT (runStateT (runReaderT ev env)) st)

eval4' :: Exp -> Eval4' Value
eval4' (Lit i)       = do
                        tick
                        return $ IntVal i
eval4' (Var n)       = do
                        tick
                        env <- ask
                        case Map.lookup n env of
                            Just val -> return val
                            Nothing  -> throwError ("unbound variable " ++ n)
eval4' (Plus e1 e2)  = do
                        tick
                        e1' <- eval4' e1
                        e2' <- eval4' e2
                        case (e1', e2') of 
                            (IntVal i1, IntVal i2) -> 
                                return $ IntVal (i1 + i2)
                            _ ->
                                throwError "type error in Plus"
eval4' (Abs n e)     = do
                        tick
                        env <- ask
                        return $ FunVal env n e
eval4' (App e1 e2)   = do
                        tick
                        env <- ask
                        val1 <- eval4' e1
                        val2 <- eval4' e2
                        case val1 of
                            FunVal env' n body ->
                                local (const (Map.insert n val2 env')) (eval4' body)
                            _ -> throwError "type error in App"


