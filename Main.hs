import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad hiding (empty)
import Control.Applicative hiding (empty)
import Data.Map as Map
import Debug.Trace
import Value

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env NullLit = return Nil
evalExpr env (VarRef (Id id)) = stateLookup env id
evalExpr env (IntLit int) = return $ Int int
evalExpr env (PrefixExpr PrefixMinus expr) = do
    exprEval <- evalExpr env expr
    case exprEval of
        (Int int) -> return $ Int (-int)
        _ -> return $ Error $ "Invalid use of prefix minus"
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    v <- stateLookup env var
    case v of
        -- Variable not defined :(
        (Error _) -> return $ Error $ (show var) ++ " not defined"
        -- Variable defined, let's set its value
        _ -> do
            e <- evalExpr env expr
            setVar var e
evalExpr env (UnaryAssignExpr inc (LVar var)) = do
    let op = case inc of
            (PrefixInc) -> OpAdd
            (PrefixDec) -> OpSub
        in
        evalExpr env (AssignExpr OpAssign (LVar var) (InfixExpr op (VarRef (Id var)) (IntLit 1)))
-- TODO(gbg): incremento e decremento pós-fixados
evalExpr env (CallExpr nameExp args) = do
    res <- evalExpr env (nameExp)
    case res of
        (Error _) -> error "deu merda"
        (Function name argsName stmts) -> ST $ \s -> 
            let (ST f) = mapM (evalExpr env) args
                (params, _) = f s
                parameters = fromList (zip (Prelude.map (\(Id a) -> a) argsName) (params))
                local = union parameters s
                (ST g) = evalStmt env (BlockStmt stmts)
                (Return val, finalState) = g local
            in (val, union (intersection (difference finalState parameters) s) s)

listVarDecl :: [Id] -> [Expression] -> [VarDecl]
listVarDecl (x:xs) (y:ys) = (VarDecl x (Just y)):(listVarDecl xs ys)
listVarDecl [] [] = []

evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (IfSingleStmt expr ifBlock) = do
    condition <- evalExpr env expr
    case condition of
        (Bool cond) -> if (cond) then do
            ret <- (evalStmt env ifBlock)
            return ret
             else (return Nil)
        error@(Error _) -> return error
evalStmt env (IfStmt expr ifBlock elseBlock) = do
    condition <- evalExpr env expr
    case condition of
        (Bool cond) -> if (cond) then do
        ret <- (evalStmt env ifBlock)
        return ret
         else do 
            ret <- (evalStmt env elseBlock)
            return ret
        error@(Error _) -> return error
evalStmt env (BlockStmt []) = return Nil
evalStmt env (BlockStmt ((BreakStmt Nothing):xs)) = return Break
evalStmt env (BlockStmt (x:xs)) = do
    ret <- evalStmt env x
    case ret of
        (Return val) -> return (Return val)
        (Break) -> return Break
        _ -> evalStmt env (BlockStmt xs)
evalStmt env (ForStmt NoInit expr1 expr2 stmt) = do
    let e1 = case expr1 of
            (Just expr) -> expr
            (Nothing) -> NullLit
        e2 = case expr2 of
            (Just expr) -> expr
            (Nothing) -> NullLit
        in do
        condition <- evalExpr env e1
        case condition of
            (Bool cond) -> if (cond) then do
                ret <- evalStmt env stmt
                case ret of
                    (Return val) -> return (Return val)
                    (Break) -> return Nil
                    _ -> do
                        evalExpr env e2
                        evalStmt env (ForStmt NoInit (Just e1) (Just e2) stmt)
                else return Nil
            (Nil) -> do
                ret <- evalStmt env stmt
                case ret of
                    (Return val) -> return (Return val)
                    (Break) -> return Nil
                    _ -> do
                        evalExpr env e2
                        evalStmt env (ForStmt NoInit (Just e1) (Just e2) stmt)
            error@(Error _) -> return error
evalStmt env (ForStmt initt expr1 expr2 stmt) = do
    let stmtIni = case initt of
            (VarInit listVarDecl) -> (VarDeclStmt listVarDecl)
            (ExprInit expr) -> (ExprStmt expr)
        in do
        evalStmt env stmtIni
        let e1 = case expr1 of
                (Just expr) -> expr
                (Nothing) -> NullLit
            e2 = case expr2 of
                (Just expr) -> expr
                (Nothing) -> NullLit
            in do
            condition <- evalExpr env e1
            case condition of
                (Bool cond) -> if (cond) then do
                    ret <- evalStmt env stmt
                    case ret of
                        Break -> return Nil
                        (Return val) -> return (Return val)
                        _ -> do
                            evalExpr env e2
                            evalStmt env (ForStmt (VarInit []) (Just e1) (Just e2) stmt)
                        else return Nil
                (Nil) -> do
                    ret <- evalStmt env stmt
                    case ret of
                        (Return val) -> return (Return val)
                        Break -> return Nil
                        _ -> do
                            evalExpr env e2
                            evalStmt env (ForStmt (VarInit []) (Just e1) (Just e2) stmt)
                error@(Error _) -> return error
-- TODO(gbg): Simplificar código do laço for
evalStmt env (ReturnStmt expression) = do
    case expression of
        (Nothing) -> return (Return Nil)
        (Just expr) -> do
            exprEval <- evalExpr env expr
            return (Return exprEval)
evalStmt env (FunctionStmt name args body) = funcDecl env (name, args, body)



-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env [stmt] = evalStmt env stmt
evaluate env (s:ss) = evalStmt env s >> evaluate env ss

--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

infixOp env op (Var x) v2 = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op val v2

infixOp env op v1 (Var x) = do
    var <- stateLookup env x
    case var of
        error@(Error _) -> return error
        val -> infixOp env op v1 val

--
-- Environment and auxiliary functions
--

environment :: Map String Value
environment = empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    (maybe
        (Error $ "Variable " ++ show var ++ " not defined")
        id
        (Map.lookup var (union s env)),
    s)

funcDecl :: StateT -> (Id, [Id], [Statement]) -> StateTransformer Value
funcDecl env ((Id id), args, stmts) = do
    setFunc id (Function (Id id) args stmts)

setFunc :: String -> Value -> StateTransformer Value
setFunc name desc = ST $ \s -> (desc, insert name desc s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id Nil
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id val

setVar :: String -> Value -> StateTransformer Value
setVar var val = ST $ \s -> (val, insert var val s)

--
-- Types and boilerplate
--

type StateT = Map String Value
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--

showResult :: (Value, StateT) -> String
showResult (val, defs) = show val ++ "\n" ++ show (toList defs) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
