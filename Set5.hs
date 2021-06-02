module Set5 where

import Data.List
import Data.Functor
import Data.Either

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Test.QuickCheck

fromLeft' :: Either l r -> l
fromLeft' (Left x) = x -- Newer GHC versions contain a fromLeft :: l -> Either l r -> l

fromRight' :: Either l r -> r
fromRight' (Right x) = x -- Newer GHC versions contain a fromRight :: r -> Either l r -> r

parser :: Parser a -> String -> a
parser p xs | isLeft res = error $ show $ fromLeft' res
          | otherwise  = fromRight' res
  where res = parse p "" xs


-- Exercise 2 defining the language
languageDef =
   emptyDef { Token.commentStart    = "/*"
            , Token.commentEnd      = "*/"
            , Token.commentLine     = "//"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = [ "for"
                                      , "while"
                                      , "if"
                                      , "then"
                                      , "else"
                                      , "dec"
                                      , "function"
                                      ]
            , Token.reservedOpNames = [ "-", "+", "*", "==", "="
                                      ]
            }

-- Create lexer (=tokenizer) for your language
lexer = Token.makeTokenParser languageDef

-- Create functions for all types of tokens
identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer


-- Exercise 3
data Expr = Const Integer
          | Call String Expr
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          | Dec Expr
          | If Condition Expr Expr
          deriving Show

parseFactor :: Parser Expr
parseFactor = Const <$> integer 
          <|> try (Call <$> identifier <*> parens parseExpr) 
          <|> Var <$> identifier 
          <|> parens parseExpr

mult :: Parser (Expr -> Expr -> Expr)
mult = (\_ -> (Mult)) <$> symbol "*"

parseTerm :: Parser Expr
parseTerm = parseFactor `chainr1` mult

add :: Parser (Expr -> Expr -> Expr)
add = (\_ -> (Add)) <$> symbol "+"

parseExpr :: Parser Expr
parseExpr = parseDec 
        <|> parseTerm `chainr1` add 
        <|> parseIf

test1 = parser parseExpr "3 * (1 + a)"

-- Exercise 4
data Condition = Eq Expr Expr deriving Show

parseCondition :: Parser Condition
parseCondition = Eq <$> parseExpr <* symbol "==" <*> parseExpr

parseIf :: Parser Expr
parseIf = If <$> (reserved "if" *> parseCondition) <*> (reserved "then" *> parseExpr) <*> (reserved "else" *> parseExpr)

parseDec :: Parser Expr
parseDec = Dec <$> (reserved "dec" *> parseExpr)

test2 = parser parseIf "if 1 == (0+1) then a else 1+1"


-- Exercise 5
data FunDef = Function String String Expr deriving Show

parseFunc :: Parser FunDef
parseFunc = Function <$> (reserved "function" *> identifier) <*> (identifier <* reserved "=") <*> parseExpr

parserFun :: String -> FunDef
parserFun = parser parseFunc 

fib :: FunDef
fib = parserFun "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"

-- fib :: Integer -> Integer
-- fib = (evalfun . parserFun)


