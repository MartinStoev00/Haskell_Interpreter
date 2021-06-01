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
data Prog = Program Function [Function]
            deriving Show

data Func = Function String [Param] Expr
            deriving Show

data Expr = Const Integer
          | Identifier String
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Call String Expr [Expr]
          | If Expr Ordering Expr Expr Expr
          | Empty
          deriving Show

data Param = Const Integer
           | Identifier String
           | None
           deriving Show

data Ordering = LT | EQ | GT
-- FP3.2
fibonacci :: Prog
fibonacci = Program  (f (Const 0) (Const 0)) 
                    [(f (Const 1) (Const 1)), 
                     (f (Identifier "n") (Add (c (Const 1)) (c (Const 2))))] 
                    where f a b = (Function "fibonacci" [a] b)
                          c d = Call "fibonacci" (Sub (Identifier "n") d)

fib :: Prog
fib = Program (Function "fib" [Identifier "n"] (If ifbool Empty ifadd)) [] 
  where ifbool = (Identifier "n") LT (Const 3)
        ifadd = AddSub ifaddl ifaddr
        ifaddl = Call "fib" (Sub (Identifier "n") (Const 1)) []
        ifaddr = Call "fib" (Sub (Identifier "n") (Const 2)) []

sum :: Prog
sum = Program (Function "sum" [Const 0] (Const 0)) [fbody] 
  where decABy1 =  Sub (Identifier "a") (Const 1)
        addDecABy1AndA = Add (Call "sum" (decABy1) [] ) (Identifier "a")
        fbody = Function "sum" [Identifier "a"] addDecABy1AndA

div :: Prog
div = Program (Function "div" [Identifier "x", Identifier "y"] (If (Identifier "x") LT (Identifier "y") (Const 0) (Add (Const 1) (Call "div" (Sub (Identifier "x") (Identifier "y")) [(Identifier "y")]))))

main :: Prog
main = Program (Function "main" [None] (Call "div" (Const 999) [(Const 2)])) []

twice :: Prog
twice = Program (Function "twice" [Identifier "f", Identifier "x"] (Call "f" (Call "f" (Identifier "x") []) [])) []

funcs :: [Prog]
funcs = [Program (Function "add" [(Identifier "x"), (Identifier "y")] (Add (Identifier "x") (Identifier "y")) [], Program (Function "inc" [None] (Call "add" (Const 1) [])) [], Program (Function "eleven" [None] (Call "inc" (Const 10) [])) []]

-- FP3.3
pretty :: Prog -> String

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
