module Eval where

import Control.Monad.State.Strict
import Control.Monad.Reader
import AST
import Atom
import Result


type EvalMonad a = StateT (NamedValueList a) Result
type EvalRevMonad a = ReaderT (NamedValueList a) Result

checkInt :: Value a -> Result Int
checkInt (VAtom (AInt i) []) = return i
checkInt v = Rejected $ "Expected Int but got " ++ show v

checkBool :: Value a -> Result Bool
checkBool (VAtom (ABool b) []) = return b
checkBool v = Rejected $ "Expected Bool but got " ++ show v

checkTuple :: Value a -> Result (NamedValueList a)
checkTuple (VSum n v) = return $ [(n, v)]
checkTuple (VProduct vs) = return $ vs
checkTuple v = Rejected $ "Expected tuple but got " ++ show v

checkFun :: Value a -> Result (Value a -> Result (Value a))
checkFun (VExpr ctx e) = return $ \a -> do
    ctx2 <- checkTuple a
    (res, _) <- eval e (ctx2 ++ ctx)
    return res
checkFun (VRevExpr ctx e) = return $ \a -> do
    t <- evalRev e a ctx
    return $ VProduct t
checkFun (VAtom op as) =
    if atomArity op < length as + 1 then Rejected $ show op ++ " takes only " ++ show (atomArity op) ++ " arguments " ++ show (length as + 1) ++ " given"
    else return $ \arg -> atomApply op (as ++ [arg])
checkFun v = Rejected $ "Expected function but got " ++ show v


atomApply :: Atom -> [Value a] -> Result (Value a)
atomApply f as | atomArity f > length as = return $ VAtom f as
atomApply AReverseFun [VExpr ctx e] = return $ VRevExpr ctx e
atomApply AReverseFun [VRevExpr ctx e] = return $ VExpr ctx e
atomApply AReverseFun [VAtom op as] = atomReverse op as
atomApply AReverseFun [v] = Rejected $ "Expected reversible function but got " ++ show v
atomApply ADupFun [v, _] = return v
atomApply ATupJoinFun [r, l] = do
    r' <- checkTuple r
    l' <- checkTuple l
    return $ VProduct $ l' ++ r'
atomApply APlusFun [r, l] = do
    r' <- checkInt r
    l' <- checkInt l
    return $ VAtom (AInt $ l' + r') []
atomApply AMinusFun [r, l] = do
    r' <- checkInt r
    l' <- checkInt l
    return $ VAtom (AInt $ l' - r') []
atomApply ATimesFun [r, l] = do
    r' <- checkInt r
    l' <- checkInt l
    return $ VAtom (AInt $ l' * r') []
atomApply ADivideFun [r, l] = do
    r' <- checkInt r
    l' <- checkInt l
    return $ VAtom (AInt $ l' `div` r') []
atomApply AEqualsFun [r, l] = do
    r' <- checkInt r
    l' <- checkInt l
    return $ VAtom (ABool $ l' == r') []
atomApply AIfFun [i, t, e] = do
    i' <- checkBool i
    return $ if i' then t else e

atomReverse :: Atom -> [Value a] -> Result (Value a)
atomReverse APlusFun [v] = return $ VAtom AMinusFun [v]
atomReverse f as = Rejected $ "Expected reversible function but got " ++ show (VAtom f as)


eval :: Expr a -> NamedValueList a -> Result (Value a, NamedValueList a)
eval e = runStateT (eval1 e)

eval1 :: Expr a -> EvalMonad a (Value a)
eval1 v = 
    mapStateT (errorHint ("while evaluating " ++ show v)) (eval' v)

eval' :: Expr a -> EvalMonad a (Value a)
eval' (ELit _ v) =
    return v
eval' (EVar _ n) = do
    ctx <- get
    (v, ctx') <- lift $ pop n ctx
    put ctx'
    return v
eval' (EApp _ f a) = do
    va <- eval1 a
    ctx <- get
    vf <- eval1 f
    put ctx
    vf' <- lift $ checkFun vf
    lift $ vf' va
eval' (ELam _ e) = do
    ctx <- get
    return $ VExpr ctx e

evalRev :: Expr a -> Value a -> NamedValueList a -> Result (NamedValueList a)
evalRev e v = runReaderT $ 
    mapReaderT (errorHint ("while matching " ++ show e ++ " against " ++ show v)) $ evalRev' e v
evalRev' :: Expr a -> Value a -> EvalRevMonad a (NamedValueList a)
evalRev' (ELit _ v') v =
    if v' == v then return []
    else lift $ Rejected $ "Expected " ++ show v' ++ " got " ++ show v
evalRev' (EVar _ n) v =
    return [(n, v)]
evalRev' (EApp x f a) v = do
    ctx <- ask
    (vf, _) <- lift $ eval (EApp x (ELit x (VAtom AReverseFun [])) f) ctx
    vf' <- lift $ checkFun vf
    r <- lift $ vf' v
    lift $ checkTuple r
evalRev' (ELam _ _) _ =
    lift $ Rejected "Cannot reverse lambda expression"
