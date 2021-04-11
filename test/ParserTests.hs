{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ParserTests where

import Data.Text
import Test.Framework
import Text.Megaparsec hiding (parseTest)
import Text.Megaparsec.Error
import Control.Monad

import Parse
import AST
import Atom


parseTest :: Text -> Expr () -> IO ()
parseTest src expected = 
    case runParser (sc *> pExpr (return (), return)) "<test>" src of
        Right v -> assertEqual expected v
        Left err -> fail $ errorBundlePretty err

bin x op l r = EApp x (EApp x (ELit x $ VAtom op []) l) r


test_parseInt = do
    parseTest "1" (ELit () $ VAtom (AInt 1) [])
    parseTest "   10   " (ELit () $ VAtom (AInt 10) [])
    parseTest "  (  (  123 ) ) " (ELit () $ VAtom (AInt 123) [])

test_parseString = do
    parseTest "\"\"" (ELit () $ VAtom (AString "") [])
    parseTest "\"\\\"\\'\\n \"" (ELit () $ VAtom (AString "\"'\n ") [])

test_parseChar = do
    parseTest "'\\0'" (ELit () $ VAtom (AChar '\0') [])
    parseTest "' '" (ELit () $ VAtom (AChar ' ') [])
    parseTest "  '\\t'  " (ELit () $ VAtom (AChar '\t') [])

test_parseFuns = do
    parseTest " a ( b ) ( c ) " (EApp () (EApp () (EVar () (U "a")) (EVar () (U "b"))) (EVar () (U "c")))
    parseTest "(a(b))(c)" (EApp () (EApp () (EVar () (U "a")) (EVar () (U "b"))) (EVar () (U "c")))
    parseTest "a((b)(c))" (EApp () (EVar () (U "a")) (EApp () (EVar () (U "b")) (EVar () (U "c"))))
    parseTest "\\a(b)(c)" (ELam () (EApp () (EApp () (EVar () (U "a")) (EVar () (U "b"))) (EVar () (U "c"))))
    parseTest " \\ \\ a ( b ) ( c ) " (ELam () (ELam () (EApp () (EApp () (EVar () (U "a")) (EVar () (U "b"))) (EVar () (U "c")))))