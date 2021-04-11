module Utils where

import Data.Text hiding (map)
import Control.Monad.Trans
import Text.Megaparsec

import AST
import Atom
import Result hiding ((>>=))
import Parse
import Eval


parseExpr :: Text -> Result (Expr ())
parseExpr text = case runParser (pExpr (return (), return)) "" text of
    Right e -> return e
    Left err -> Rejected $ errorBundlePretty err

defaultContext :: NamedValueList ()
defaultContext =
    map (\(n, a) -> (U n, VAtom a [])) defaultAtoms ++
    [(U "unit", VProduct [])]

evalExpr :: Text -> Result (Value (), NamedValueList ())
evalExpr t = parseExpr t >>= (`eval` defaultContext)