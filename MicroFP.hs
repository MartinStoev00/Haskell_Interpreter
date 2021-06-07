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
import System.FilePath

-- FP3.3
-- This exercise continues below where the "p_" functions are defined. 
-- For the sake of organization the class and instances were defined at the top of the file
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

-- FP3.1
data Prog = Program Func [Func]
            deriving (Show, Eq)

data Func = Function String [Param] Expr
            deriving (Show, Eq)

data Expr = Number Integer
          | Identifier String
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Call String [Expr]
          | If Cond Expr Expr
          | None
          deriving (Show, Eq)

data Cond = Condition Expr Ords Expr
            deriving (Show, Eq)

data Param = Num Integer
           | Id String
           deriving (Show, Eq)

data Ords = LesserThan String | EqualTo String | GreaterThan String
            deriving (Show, Eq)

-- FP5.6 
instance Arbitrary Prog where
      arbitrary = Program <$> f <*> fs
            where f     = arbitrary :: Gen Func
                  fs    = resize 3 $ (arbitrary :: Gen [Func])

instance Arbitrary Func where
      arbitrary = Function <$> genName <*> params <*> expression
            where genName     = suchThat funcStr correctLength
                  params      = resize 3 $ (arbitrary :: Gen [Param])
                  expression  = arbitrary :: Gen Expr

instance Arbitrary Param where
      arbitrary = oneof [Num   <$>  suchThat (arbitrary :: Gen Integer) isPositive
                        , Id    <$> suchThat identifierStr correctLength]

instance Arbitrary Cond where
      arbitrary = Condition <$> (arbitrary :: Gen Expr) <*> (arbitrary :: Gen Ords) <*> (arbitrary :: Gen Expr)

instance Arbitrary Ords where
      arbitrary = oneof [GreaterThan <$> suchThat ordStr greaterThan, EqualTo <$> suchThat ordStr equalTo, LesserThan <$> suchThat ordStr lessThan] 

instance Arbitrary Expr where
      arbitrary = frequency [(10, Number <$> suchThat (arbitrary :: Gen Integer) isPositive)
              ,(10, Identifier <$> suchThat identifierStr correctLength)
              ,(1, Add         <$> (arbitrary :: Gen Expr) <*> (arbitrary :: Gen Expr))
              ,(1, Sub         <$> (arbitrary :: Gen Expr) <*> (arbitrary :: Gen Expr))
              ,(1, Mult        <$> (arbitrary :: Gen Expr) <*> (arbitrary :: Gen Expr))
              ,(1, Call        <$> suchThat funcStr correctLength <*> suchThat (resize 3 (arbitrary :: Gen [Expr])) atLeastOne)
              ,(1, If          <$> (arbitrary :: Gen Cond) <*> (arbitrary :: Gen Expr) <*> (arbitrary :: Gen Expr))]


-- The following are lists that were used for generation of certain data structures.
ordStr :: Gen String
ordStr = listOf $ elements ">=<"

funcStr :: Gen String
funcStr = listOf $ elements "fghk"

identifierStr :: Gen String
identifierStr = listOf $ elements "abcde"

-- These are the boolean expressions which were used to ensure that we could generate random structures with specific requirements
greaterThan :: String -> Bool
greaterThan str = str == ">"

equalTo :: String -> Bool
equalTo str = str == "=="

lessThan :: String -> Bool
lessThan str = str == "<"

correctLength :: String -> Bool
correctLength str = (length str) == 3

atLeastOne :: [a] -> Bool
atLeastOne list = length list >= 1

isPositive :: Integer -> Bool
isPositive x = x >= 0

-- we make it so that we compare the pretty string formats of the generated structure, as our compiler can compile a string in a different 
-- but equal format from the generated one, so in order to compare if they are indeed the same, we convert them both to strings for comparison.

-- tests if a generated expression can be converted into a string and then compiled and still have the same value. 
prop_GenExpr :: Expr -> Bool
prop_GenExpr expression = (pretty expression) == (pretty (compileExpr (pretty expression)))

-- tests if a generated program can be converted into a string and then compiled and still have the same value. 
prop_GenProg :: Prog -> Bool
prop_GenProg program = (pretty program) == (pretty (compile (pretty program)))

-- Generating a random expression
genExpr = generate (arbitrary :: Gen Expr)
-- Generating a random program
genProg = generate (arbitrary :: Gen Prog)

-- Testing for FP5.6: Methods to test quickcheck with our arbitrary implementations.
testExpr = quickCheck prop_GenExpr
testProg = quickCheck prop_GenProg


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

-- The following functions are defined seperatly from the Prog "func" for readability
funcsAdd :: Func
funcsAdd = Function "add" [(Id "x"), (Id "y")] (Add (Identifier "x") (Identifier "y"))

funcsAddInc :: Func
funcsAddInc = Function "addinc" [(Id "x"), (Id "y")] (Add (Number 1) (Call "add" [(Identifier "x"), (Identifier "y")]))

funcsInc :: Func
funcsInc = Function "inc" [] (Call "add" [(Number 1)])

funcsEleven :: Func
funcsEleven = Function "eleven" [] (Call "inc" [(Number 10)])

funcs :: Prog
funcs = Program (funcsAdd) [funcsInc, funcsAddInc, funcsEleven]

      -- We are not sure if there is a specific function or any test that would be useful to show that these are defined correctly.
      -- refer to FP3.3 to see these functions printed.

-- FP3.3
-- NOTE: Also includes the type class Pretty and its instances defined at the top of the file.

-- pretty program
pp :: Prog -> String
pp (Program f []) = (pf f) ++ "\n"
pp (Program f fs) = (pf f) ++ (foldl (\f o -> (f ++ "\n" ++(pf o))) "" fs) ++ "\n"

-- pretty function
pf :: Func -> String
pf (Function name params exp) = name ++ (foldl (\f o -> (f ++ " " ++ (prettyParam o))) "" params) ++ " := "  ++ (pe exp) ++ [';']

-- pretty expression
pe :: Expr -> String
pe (Number x) = show x :: String
pe (Identifier s) = s 
pe (Add e1 e2) = (pe e1) ++ " + " ++ (pe e2)
pe (Sub e1 e2) = (pe e1) ++ ['-'] ++ (pe e2)
pe (Mult e1 e2) = (pe e1) ++ " * " ++ (pe e2)
pe (Call na (c1:c2)) = na ++ " (" ++ (pe c1) ++ (foldl (\f o -> (f ++ ", " ++ (pe o))) "" c2) ++ [')']
pe (If c1 e3 e4) = "if (" ++ (pc c1) ++ ") then {\n\t\t" ++ (pe e3) ++ "\n\t} else {" ++ "\n\t\t"++(pe e4) ++ "\n\t}"

-- pretty ordering
po :: Ords-> String
po (LesserThan x) = " " ++ x ++ " "
po (EqualTo x) = " " ++ x ++ " "
po (GreaterThan x) = " " ++ x ++ " "

-- pretty condition
pc :: Cond -> String
pc (Condition e1 o e2) = (pe e1) ++ (po o) ++ (pe e2)

-- pretty parameters
prettyParam :: Param -> String
prettyParam (Num x) = show x
prettyParam (Id x) = x

-- Print function
pPrint :: Pretty a => a -> IO ()
pPrint input = putStr (pretty input)

-- Example uses of pretty to get a string version of any part of the EDSL: 
prettyFib = pretty fib
prettyAdd = pretty (Add (Identifier "a") (Identifier "b"))

-- Some example uses of pPrint to showcase our pretty function with whitespace included
printDiv = pPrint divProg
printFib = pPrint fib
printFibonacci = pPrint fibonacci

-- FP3.4
-- Various methods for obtaining necessary parts of a function or program for usage in later functions.
getFunctionName :: Func -> String
getFunctionName (Function fname pars exp) = fname

getFunctionList :: String -> [Func] -> Func
getFunctionList _ [] = error("No Such Function")
getFunctionList fname (a:res) 
                  | fname == (getFunctionName a)  = a
                  | otherwise                     = getFunctionList fname res

getFunctionFromName :: String -> Prog -> Func
getFunctionFromName fname (Program f res) 
                  | fname == (getFunctionName f) = f
                  | otherwise                    = getFunctionList fname res

getLastFunction :: Prog -> String
getLastFunction (Program f []) = getFunctionName f
getLastFunction (Program f fs) = getFunctionName (last fs)

-- Utility helpers for the evaluation function
bind :: Func -> [Integer] -> [(String, Integer)]
bind (Function fname pars exp) args = zipWith (\x y -> ((prettyParam x), y)) pars args

replace :: [(String, Integer)] -> Expr -> Integer
replace [] _ = error("No Such Value Exists")
replace (pair:binds) (Identifier x)      
                              | x == (fst pair)  = snd pair
                              | otherwise        = replace binds (Identifier x)

-- Evaluators for each part of the EDSL
evalExpr :: Prog -> [(String, Integer)] -> Expr -> Integer
evalExpr _ binds (Number x) = x
evalExpr _ binds (Identifier x) = (replace binds (Identifier x))
evalExpr p binds (Add e1 e2) = (evalExpr p binds e1) + (evalExpr p binds e2)
evalExpr p binds (Sub e1 e2) = (evalExpr p binds e1) - (evalExpr p binds e2)
evalExpr p binds (Mult e1 e2) = (evalExpr p binds e1) * (evalExpr p binds e2)
evalExpr p binds (Call fname args) = eval p fname (map (\x -> evalExpr p binds x) args)
evalExpr p binds (If cond e1 e2) = if (evalCondition p binds cond) then (evalExpr p binds e1) else (evalExpr p binds e2)

evalCondition :: Prog -> [(String, Integer)] -> Cond -> Bool
evalCondition p binds (Condition e1 (LesserThan _) e2) = (evalExpr p binds e1) < (evalExpr p binds e2)
evalCondition p binds (Condition e1 (EqualTo _) e2) = (evalExpr p binds e1) == (evalExpr p binds e2)
evalCondition p binds (Condition e1 (GreaterThan _) e2) = (evalExpr p binds e1) > (evalExpr p binds e2)

evalFunction :: Prog -> [Integer] -> Func -> Integer
evalFunction p args (Function name a e) = evalExpr p (bind (Function name a e) args) e

eval :: Prog -> String -> [Integer] -> Integer
eval prog fname args  = evalFunction prog args (getFunctionFromName fname prog)

-- Test functions for the evaluator. Only works for the most simple functions. No pattern matching or other more complex evaluator was built.
evalAdd = eval funcs "add" [999, 2]
evalDiv = eval divProg "div" [100, 4]
evalFib = eval fib "fib" [10]

-- FP4.1
-- Below are all of the parsers we defined for the grammar.

program :: Parser Prog
program = Program <$> function' <*> many function'

      -- NOTE: named function' because function as a name was clashing with something in haskell
function' :: Parser Func
function' = Function <$> identifier <*> many param <*> (symbol ":=" *> expr) <* symbol ";"

param :: Parser Param
param = Id <$> identifier 
    <|> Num <$> integer

expr :: Parser Expr
expr = Add <$> (term <* symbol "+") <*> expr
   <|> Sub <$> (term <* symbol "-") <*> expr
   <|> term

term :: Parser Expr
term = Mult <$> (factor <* symbol "*") <*> term 
   <|> factor 

factor :: Parser Expr
factor = Number         <$> integer 
     <|> Call           <$> identifier <*> parens (sep expr (symbol ","))
     <|> If             <$> (symbol "if" *> (parens condition <* symbol "then")) <*> (braces expr) <*> (symbol "else" *> (braces expr))
     <|> Identifier     <$> identifier
     <|> parens expr     

condition :: Parser Cond
condition = Condition <$> expr <*> ordering <*> expr

ordering :: Parser Ords
ordering = GreaterThan <$> symbol ">"
      <|> EqualTo      <$> symbol "=="
      <|> LesserThan   <$> symbol "<"


-- Testing for 4.1

-- Gives the entire parsing output.
parseProgram p = runParser program (Stream p)
-- Gives only what was parsed in the input.
parseProgram' p = fst (head (runParser program (Stream p)))

-- Use these for testing: (optionally remove the ' from the parseProgram to see the entire output)
parseFuncs = parseProgram' (pretty funcs)
parseMain = parseProgram' (pretty main)
parseTwice = parseProgram' (pretty twice)

-- FP4.2
-- Compiling an entire program
compile :: String -> Prog
compile str = fst (head (runParser program (Stream str)))

-- Compiling only an expresssion
compileExpr :: String -> Expr
compileExpr str = fst (head (runParser expr (Stream str)))

-- Testing for FP4.2:
testComp = compile (pretty fib)
testExprComp = compileExpr (pretty (Add (Number 0) (Number (1))))

-- FP4.3
runFile :: FilePath -> [Integer] -> IO Integer
runFile path args = eval <$> prog <*> fname <*> pure args
                        where prog = fmap compile (readFile path)
                              fname = fmap getLastFunction prog

-- testing for FP4.3 (test.txt is a file with the add function at the end of it)
testRun = runFile "test.txt" [999, 1]


-- FP5.6 Continuted. Further testing for QuickCheck: all prop_* tests
return []
check = $quickCheckAll