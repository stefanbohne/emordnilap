module AST where

import Result
import Atom
import Data.List
import Data.Char


data Name = D | U String
    deriving Eq
instance Show Name where
    show D = "*"
    show (U n) = if all (\ch -> isAlphaNum ch || ch == '_') n then n
                 else "`" ++ (init $ tail $ show n) ++ "`"

type NamedList a = [(Name, a)]
push :: Name -> a -> NamedList a -> Result (NamedList a)
push n v r = return ((n, v) : r)
pushAll :: NamedList a -> NamedList a -> Result (NamedList a)
pushAll a b = return $ b ++ a 
pop :: Name -> NamedList a -> Result (a, NamedList a)
pop  n' ((n, v) : r) =
    if n == n' then return (v, r)
    else do (v', r') <- pop n' r; return (v', (n, v) : r')
pop n' [] =
    Rejected $ "Variable " ++ show n' ++ " not found"

type NamedTypeList = NamedList Type
type NamedValueList a = NamedList (Value a)


data JanusClass =
        JCFun
    |   JCRev
    deriving (Eq)
data Type =
        TInt
    |   TBool
    |   TString
    |   TChar
    |   TFun JanusClass Type Type
    |   TForall Name Type Type
    |   TVar Name
    |   TProduct NamedTypeList
    |   TSum NamedTypeList
    deriving (Eq)

data Value a =
        VAtom Atom [Value a]
    |   VSum Name (Value a)
    |   VProduct (NamedValueList a)
    |   VExpr (NamedValueList a) (Expr a)
    |   VRevExpr (NamedValueList a) (Expr a)
    deriving (Eq)
instance Show (Value a) where
    show (VAtom a l) = show a ++ intercalate "" (map showl l)
        where showl v = "(" ++ show v ++ ")"
    show (VSum n v) = show n ++ "(" ++ show v ++ ")"
    show (VProduct vs) = "(" ++ intercalate ", " (map showvs vs) ++ ")"
        where showvs (n, v) = show n ++ " = " ++ show v
    show (VExpr ctx e) = "(\\" ++ show e ++ ")"
    show (VRevExpr ctx e) = "`~`(\\" ++ show e ++ ")"

data Expr a =
        EApp a (Expr a) (Expr a)
    |   ELam a (Expr a)
    |   EVar a Name
    |   ELit a (Value a)
instance Eq (Expr a) where
    (EApp _ f a) == (EApp _ f' a') = f == f' && a == a'
    (ELam _ b) == (ELam _ b') = b == b'
    (EVar _ n) == (EVar _ n') = n == n'
    (ELit _ v) == (ELit _ v') = v == v'
    _ == _ = False
instance Show (Expr a) where
    show (EApp _ f a) = show f ++ "(" ++ show a ++ ")"
    show (ELam _ e) = "(\\" ++ show e ++ ")"
    show (EVar _ n) = show n
    show (ELit _ v) = show v

class Annotated f where
    annotation :: f a -> a
instance Annotated Expr where
    annotation (EApp x _ _) = x
    annotation (ELam x _) = x
    annotation (EVar x _) = x
    annotation (ELit x _) = x
