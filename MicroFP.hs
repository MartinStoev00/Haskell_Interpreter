-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Martin Stoev (s2392593)
-- Student 2: Remy Benitah (s2247372)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All

-- FP3.1
class Pretty a where 
      pretty :: a -> String

instance Pretty Prog where
      pretty = pp
instance Pretty Func where
      pretty = pf
instance Pretty Expr where
      pretty = pe
instance Pretty Ords where
      pretty = po
instance Pretty Param where
      pretty = prettyParam

data Prog = Program Func [Func]
            deriving Show

data Func = Function String [Param] Expr
            deriving Show

data Expr = Number Integer
          | Identifier String
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Call String [Expr]
          | If Cond Expr Expr
          deriving Show

data Cond = Condition Expr Ords Expr
            deriving Show

data Param = Num Integer
           | Id String
           deriving Show

data Ords = LesserThan String | EqualTo String | GreaterThan String
            deriving Show

-- FP3.2
fibonacci :: Prog
fibonacci = Program  (f (Num 0) (Number 0)) [(f (Num 1) (Number 1)), (f (Id "n") (Add (Call "fibonacci" [(Sub (Identifier "n") (Number 1))]) (Call "fibonacci" [(Sub (Identifier "n") (Number 2))])))] 
                    where f a b = (Function "fibonacci" [a] b)
fib :: Prog
fib = Program (Function "fib" [Id "n"] (If (Condition (Identifier "n") (LesserThan "<") (Number 3)) (Number 1) (Add (Call "fib" [(Sub (Identifier "n") (Number 1))]) (Call "fib" [Sub (Identifier "n") (Number 2)])))) [] 

sum :: Prog
sum = Program (Function "sum" [Num 0] (Number 0)) [fbody] 
  where decABy1 =  Sub (Identifier "a") (Number 1)
        addDecABy1AndA = Add (Call "sum" [(decABy1)]) (Identifier "a")
        fbody = Function "sum" [Id "a"] addDecABy1AndA

divProg :: Prog
divProg = Program (Function "div" [(Id "x"), (Id "y")] (If (Condition (Identifier "x") (LesserThan "<") (Identifier "y")) (Number 0) (Add (Number 1) (Call "div" [(Sub (Identifier "x") (Identifier "y")), (Identifier "y")])))) []

main :: Prog
main = Program (Function "main" [] (Call "div" [(Number 999), (Number 2)])) []

twice :: Prog
twice = Program (Function "twice" [(Id "f"), (Id "x")] (Call "f" [(Call "f" [(Identifier "x")])])) []

funcs :: [Prog]
funcs = [Program (Function "add" [(Id "x"), (Id "y")] (Add (Identifier "x") (Identifier "y"))) [], Program (Function "inc" [] (Call "add" [(Number 1)])) [], Program (Function "eleven" [] (Call "inc" [(Number 10)])) []]

-- FP3.3
-- NOTE: Also includes the type class Pretty and its instances defined at the top of the file.
pp :: Prog -> String
pp (Program f []) = (pf f) ++ "\n"
pp (Program f fs) = (pf f) ++ (foldl (\f o -> (f ++ "\n" ++(pf o))) "" fs) ++ "\n"


pf :: Func -> String
pf (Function name params exp) = name ++ (foldl (\f o -> (f ++ " " ++ (prettyParam o))) "" params) ++ " := "  ++ (pe exp) ++ [';']

pe :: Expr -> String
pe (Number x) = show x
pe (Identifier s) = s 
pe (Add e1 e2) = (pe e1) ++ " + " ++ (pe e2)
pe (Sub e1 e2) = (pe e1) ++ ['-'] ++ (pe e2)
pe (Mult e1 e2) = (pe e1) ++ " * " ++ (pe e2)
pe (Call na (c1:c2)) = na ++ " (" ++ (pe c1) ++ (foldl (\f o -> (f ++ ", " ++ (pe o))) "" c2) ++ [')']
pe (If c1 e3 e4) = "if (" ++ (pc c1) ++ ") then {\n\t\t" ++ (pe e3) ++ "\n\t} else {" ++ "\n\t\t"++(pe e4) ++ "\n\t}"

po :: Ords-> String
po (LesserThan x) = " " ++ x ++ " "
po (EqualTo x) = " " ++ x ++ " "
po (GreaterThan x) = " " ++ x ++ " "

pc :: Cond -> String
pc (Condition e1 o e2) = (pe e1) ++ " " ++ (po o) ++ " " ++ (pe e2)

prettyParam :: Param -> String
prettyParam (Num x) = show x
prettyParam (Id x) = x

-- PRINT function.
pPrint :: Pretty a => a -> IO ()
pPrint input = putStr (pretty input)


-- FP3.4



-- FP4.1
program :: Parser Prog
program = Program <$> function' <*> many function'

function' :: Parser Func
function' = Function <$> identifier <*> many param <*> (symbol ":=" *> expr) <* symbol ";"

param :: Parser Param
param = Id <$> identifier 
    <|> Num <$> integer

expr :: Parser Expr
expr = term 
      <|> Add <$> term <* symbol "+" *> expr
      <|> Sub <$> term <* symbol "-" *> expr

term :: Parser Expr
term = factor 
   <|> Mult <$> factor <* symbol "*" *> term


factor :: Parser Expr
factor = Number   <$> integer 
     <|> Call     <$> identifier <*> parens (sep expr (symbol ",") )
     <|> If       <$> (symbol "if" *> parens condition) <* symbol "then" *> (braces expr) <* symbol "else" *> (braces expr)
     <|> parens expr

condition :: Parser Cond
condition = Condition <$> expr <*> ordering <*> expr

ordering :: Parser Ords
ordering = GreaterThan  <$> symbol ">"
       <|> EqualTo      <$> symbol "=="  
       <|> LesserThan   <$> symbol "<"  


-- FP4.2
mainStr = "main := div (999, 2);"
sumStr = "sum 0 := 0;\nsum a := sum (a-1) + a;"

-- compile :: String -> Prog
-- compile [] = []
-- compile 


-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
