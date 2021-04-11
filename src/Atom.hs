module Atom where

import Result


data Atom =
        AReverseFun
    |   ADupFun
    |   ATupJoinFun
    |   APlusFun
    |   AMinusFun
    |   ATimesFun
    |   ADivideFun
    |   AEqualsFun
    |   AIfFun
    |   AInt Int
    |   ABool Bool
    |   AString String
    |   AChar Char
    deriving (Eq)
instance Show Atom where
    show AReverseFun = "`~`"
    show ADupFun = "dup"
    show ATupJoinFun = "`,`"
    show APlusFun = "`+`"
    show AMinusFun = "`-`"
    show ATimesFun = "`*`"
    show ADivideFun = "`/`"
    show AEqualsFun = "`==`"
    show AIfFun = "if"
    show (AInt i) = show i
    show (ABool True) = "true"
    show (ABool False) = "false"
    show (AString s) = show s

atomArity :: Atom -> Int
atomArity AReverseFun = 1
atomArity ADupFun = 2
atomArity ATupJoinFun = 2
atomArity APlusFun = 2
atomArity AMinusFun = 2
atomArity ATimesFun = 2
atomArity ADivideFun = 2
atomArity AEqualsFun = 2
atomArity AIfFun = 3
atomArity _ = 0

defaultAtoms = [("~", AReverseFun),
                ("dup", ADupFun),
                (",", ATupJoinFun),
                ("+", APlusFun),
                ("-", AMinusFun),
                ("*", ATimesFun),
                ("/", ADivideFun),
                ("==", AEqualsFun),
                ("true", ABool True),
                ("false", ABool False),
                ("if", AIfFun)]