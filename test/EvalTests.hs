{-# OPTIONS_GHC -F -pgmF htfpp #-}
module EvalTests where

import Data.Text
import Test.Framework hiding (Success)
import Text.Megaparsec.Error
import Control.Monad

import AST
import Atom
import Eval
import Result
import Utils


evalTest :: Text -> Value () -> IO ()
evalTest src expected = do
    assertEqual (Success expected) (fmap fst $ evalExpr src)

evalTestRejected :: Text -> IO ()
evalTestRejected src = do
    case evalExpr src of
        Rejected _ -> return ()

testAtoms = do
    forM defaultContext $ \(U n, v) -> evalTest (pack n) v

testMath = do
    evalTest "`+`(1)(1)" (VAtom (AInt 2) [])
    evalTest "`-`(1)(1)" (VAtom (AInt 0) [])
    evalTest "`*`(2)(3)" (VAtom (AInt 6) [])
    evalTest "`/`(2)(3)" (VAtom (AInt 1) [])
