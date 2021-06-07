-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Martin Stoev (s2392593)
-- Student 2: Remy Benitah (s2247372)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- FP1.1 
-- Creates a stream of characters to be parsed by the Parser
data Stream = Stream [Char]
              deriving (Eq, Show)
s = Stream "text to be parsed"

-- Creates a parser with a method runParser that takes in a stream 
-- and returns a list of tuples with the first element being 
-- the parsed result, and the second element being the character(s)
-- of the stream that have not been parsed 
data Parser r = P { 
    runParser :: Stream -> [(r, Stream)]
}

-- FP1.2
-- Created a Functor instance of Parser in order for the parsed
-- result to be mapped onto something different that might be needed
instance Functor Parser where
  fmap f p = P (\x -> [(f r, s) | (r, s) <- runParser p x])

-- Creates Parser for 'a' which is then concatenated to "123"
fmapTest = fmap (\x -> x:"123") (char 'a')

-- FP1.3
-- Parses a single character if the list is empty or if it fails to parse.
-- If it parses, it returns a list with a tuple with the parsed result
-- and the second element being the rest of the unparsed stream
char :: Char -> Parser Char
char c = P p 
    where
        p (Stream [])                   = []
        p (Stream (x:xs))   | c == x    = [(x, (Stream xs))]
                            | otherwise = []
-- Given a String, this funciton will parse it and check 
-- if it begins with the character 'a'
charAParser a = runParser (char 'A') (Stream a)

-- FP1.4
-- Always returns an empty array which is considered 
-- a failure to parse
failure :: Parser a 
failure = P (\_ -> [])
failParse a = runParser failure (Stream a)

-- FP1.5
-- Creates an Applicative instance of Parser which allows 
-- for another parser to be used on the remainder of the Stream
instance Applicative Parser where
    pure x  = P (\stream -> [(x, stream)])   -- pure
    x <*> y = P (\stream -> [(r p, s') | (r, s) <- runParser x stream, (p, s') <- runParser y s]) -- composition

pureEx = runParser (pure 42) (Stream "123")

-- FP1.6
-- Created an Alternative instance of Parser which uses
-- Parsers until one of the is able to parse the input stream 
instance Alternative Parser where
    empty = P (\_ -> [])
    p1 <|> p2 = P p 
        where
            p stream = if (null firstP) then (runParser p2 stream) else (firstP)
                where 
                    firstP = runParser p1 stream 

appEx = runParser ((,) <$> char 'a' <*> char 'b') (Stream "ab")