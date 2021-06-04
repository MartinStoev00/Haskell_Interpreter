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
-- FP3.3 continues below where the functions are defined.
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
              ,(1, Call        <$> suchThat funcStr correctLength <*> resize 3 (arbitrary :: Gen [Expr]))
              ,(1, If          <$> (arbitrary :: Gen Cond) <*> (arbitrary :: Gen Expr) <*> (arbitrary :: Gen Expr))]


ordStr :: Gen String
ordStr = listOf $ elements ">=<"

greaterThan :: String -> Bool
greaterThan str = str == ">"

equalTo :: String -> Bool
equalTo str = str == "=="

lessThan :: String -> Bool
lessThan str = str == "<"

funcStr :: Gen String
funcStr = listOf $ elements "fghk"

identifierStr :: Gen String
identifierStr = listOf $ elements "abcde"

correctLength :: String -> Bool
correctLength str = (length str) == 3

isPositive :: Integer -> Bool
isPositive x = x >= 0

prop_GenExpr :: Expr -> Bool
prop_GenExpr expression = expression == (compileExpr (pretty expression))

prop_GenProg :: Prog -> Bool
prop_GenProg program = program == (compile (pretty program))

testEq program = program == (compile (pretty program))

genX = generate (arbitrary :: Gen Expr)
-- generate (arbitrary :: Gen Expr)
-- generate (arbitrary :: Gen Prog)

-- FP3.2
fibonacci :: Prog
fibonacci = Program  (f (Num 0) (Number 0)) [(f (Num 1) (Number 1)), (f (Id "n") (Add (Call "fibonacci" [(Sub (Identifier "n") (Number 1))]) (Call "fibonacci" [(Sub (Identifier "n") (Number 2))])))] 
                    where f a b = (Function "fibonacci" [a] b)


tt :: Prog
tt = Program  (Function "s" [] (Number 1)) [(Function "t" [] (Number 2)),(Function "d" [] (Number 3))]

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

-- FP3.3
-- NOTE: Also includes the type class Pretty and its instances defined at the top of the file.
pp :: Prog -> String
pp (Program f []) = (pf f) ++ "\n"
pp (Program f fs) = (pf f) ++ (foldl (\f o -> (f ++ "\n" ++(pf o))) "" fs) ++ "\n"

pf :: Func -> String
pf (Function name params exp) = name ++ (foldl (\f o -> (f ++ " " ++ (prettyParam o))) "" params) ++ " := "  ++ (pe exp) ++ [';']

pe :: Expr -> String
pe (Number x) = show x :: String
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
pc (Condition e1 o e2) = (pe e1) ++ (po o) ++ (pe e2)

prettyParam :: Param -> String
prettyParam (Num x) = show x
prettyParam (Id x) = x

-- Print function
pPrint :: Pretty a => a -> IO ()
pPrint input = putStr (pretty input)


-- FP3.4
bind :: Func -> [Integer] -> [(String, Integer)]
bind (Function fname pars exp) args = zipWith (\x y -> ((prettyParam x), y)) pars args

replace :: [(String, Integer)] -> Expr -> Integer
replace [] _ = error("No Such Value Exists")
replace (pair:binds) (Identifier x)      
                              | x == (fst pair)  = snd pair
                              | otherwise        = replace binds (Identifier x)

evalExpr :: Prog -> [(String, Integer)] -> Expr -> Integer
evalExpr _ binds (Number x) = x
evalExpr _ binds (Identifier x) = (replace binds (Identifier x))
evalExpr p binds (Add e1 e2) = (evalExpr p binds e1) + (evalExpr p binds e2)
evalExpr p binds (Sub e1 e2) = (evalExpr p binds e1) - (evalExpr p binds e2)
evalExpr p binds (Mult e1 e2) = (evalExpr p binds e1) * (evalExpr p binds e2)
evalExpr p binds (Call fname args) = eval p fname (map (\x -> evalExpr p binds x) args)
evalExpr p binds (If cond e1 e2) = if (evalCondition p binds cond) then (evalExpr p binds e1) else (evalExpr p binds e2)
-- TODO: is this needed?

evalCondition :: Prog -> [(String, Integer)] -> Cond -> Bool
evalCondition p binds (Condition e1 (LesserThan _) e2) = (evalExpr p binds e1) < (evalExpr p binds e2)
evalCondition p binds (Condition e1 (EqualTo _) e2) = (evalExpr p binds e1) == (evalExpr p binds e2)
evalCondition p binds (Condition e1 (GreaterThan _) e2) = (evalExpr p binds e1) > (evalExpr p binds e2)

evalFunction :: Prog -> [Integer] -> Func -> Integer
evalFunction p args (Function name a e) = evalExpr p (bind (Function name a e) args) e

eval :: Prog -> String -> [Integer] -> Integer
eval prog fname args  = evalFunction prog args (getFunctionFromName fname prog)

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

-- FP4.1
program :: Parser Prog
program = Program <$> function' <*> many function'

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

-- TODO: Look into combining call and Identifier using the option parser.
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


-- testing for 4.1
mainStr = "main := div (999, 2);"
sumStr = "sum 0 := 0;\nsum a := sum (a-1) + a;"
sum2 = "sum a := sum (a-1) + a;"
addStr = "add x y := x + y;"
parseProgram p = runParser program (Stream p)
parseProgram' p = fst (head (runParser program (Stream p)))

-- filename -> stringreadfromfile -> compile file -> Program (f1) [f2,f3,f4...]
testParsing x y = show (parseProgram x) == pretty y

-- FP4.2
compile :: String -> Prog
compile str = fst (head (runParser program (Stream str)))

compileExpr :: String -> Expr
compileExpr str = fst (head (runParser expr (Stream str)))

-- compile' :: String -> [String, (Expr, Stream)]
compile' str = (str, (runParser expr (Stream str)))

-- FP4.3
runFile :: FilePath -> [Integer] -> IO Integer
runFile path args = eval <$> prog <*> fname <*> pure args
                        where prog = fmap compile (readFile path)
                              fname = fmap getLastFunction prog

-- QuickCheck: all prop_* tests
-- return []
-- check = $quickCheckAll
