-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Martin Stoev (s2392593)
-- Student 2: Remy Benitah (s2247372)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- FP1.1 
data Stream = Stream [Char]
              deriving (Eq, Show)

data Parser r = P { 
    runParser :: Stream -> [(r, Stream)]
}

-- FP1.2
instance Functor Parser where
  fmap f p = P (\x -> [(f r, s) | (r, s) <- runParser p x])       


-- FP1.3
char :: Char -> Parser Char
char c = P p 
    where
        p (Stream [])                   = []
        p (Stream (x:xs))   | c == x    = [(x, (Stream xs))]
                            | otherwise = []

-- Testing:
testChar = runParser (char 's') (Stream "saa")

-- FP1.4
failure :: Parser a 
failure = P (\_ -> [])

-- FP1.5
instance Applicative Parser where
    pure x  = P (\stream -> [(x, stream)])   -- pure
    x <*> y = P (\stream -> [(r p, s') | (r, s) <- runParser x stream, (p, s') <- runParser y s]) -- composition

-- Testing:
-- runParser testApp (Stream "saa")
testApp = (\x y -> [x,y]) <$> (char 's') <*> (char 'a') z
-- testApp = (\x y z -> y) <$> (char 's') <*> (char 'a') <*> (char 'a')

-- FP1.6
instance Alternative Parser where
    empty = P (\_ -> [])
    p1 <|> p2 = P (\stream -> if (null firstP) 
        then (runParser p2 stream) 
        else (firstP)
        where firstP = runParser p1 stream 

-- runParser (char 'a' <|> char 'b') (Stream "ba")