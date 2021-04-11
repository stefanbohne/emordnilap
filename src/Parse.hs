{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Data.Text (Text)
import Data.Void

import AST
import Atom
import Result

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

operator :: Text -> Parser ()
operator text = do
    string text
    notFollowedBy $ oneOf ("!@#$%^&*-+=<>|/\\~:;" :: String)
    sc
    return ()

pIdentifier :: Parser Name
pIdentifier = lexeme (U <$> p <?> "variable")
    where p  =  ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
            <|> (char '`' *> manyTill L.charLiteral (char '`'))

pAtom :: Parser Atom
pAtom =
        (AInt <$> lexeme L.decimal <?> "integer literal")
    <|> (AString <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"')) <?> "string literal")
    <|> (AChar <$> lexeme (between (char '\'') (char '\'') L.charLiteral) <?> "character literal")

pExpr :: (Parser a, a -> Parser b) -> Parser (Expr b)
pExpr (before, after) = do
        be <- before
        f <- pExprTerm (before, after)
        as <- many (pArg be)
        return $ foldl (flip ($)) f as
    where
        pArg be = do
            a <- between (symbol "(") (symbol ")") (pExpr (before, after))
            af <- after be
            return $ \f -> EApp af f a
pExprTerm :: (Parser a, a -> Parser b) -> Parser (Expr b)
pExprTerm (before, after) = do
        be <- before
        pLit be <|> pVar be <|> pLam be <|>
            between (symbol "(") (symbol ")") (pExpr (before, after))
    where
        pLit be = do
            v <- VAtom <$> pAtom <*> return []
            af <- after be
            return $ ELit af v
        pVar be = do
            n <- pIdentifier
            af <- after be
            return $ EVar af n
        pLam be = do
            b <- operator "\\" *> pExpr (before, after)
            af <- after be
            return $ ELam af b

data ExprPos = ExprPos {
    start :: SourcePos,
    end :: SourcePos
    }
    deriving (Show, Eq)

exprPos :: (Parser SourcePos, SourcePos -> Parser ExprPos)
exprPos = (getSourcePos, \start -> ExprPos start <$> getSourcePos)