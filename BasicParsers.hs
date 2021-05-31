-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Martin Stoev (s2392593)
-- Student 2: Remy Benitah (s2247372)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb

-- FP2.1
letters = ['a'..'z'] ++ ['A'..'Z']
digits = ['0'..'9']
-- parses any character
letter :: Parser Char
letter = P p 
    where p (Stream [])                        = []
          p (Stream (x:xs)) | x `elem` letters = [(x, Stream xs)]
                            | otherwise        = []
-- parses any digit
dig :: Parser Char
dig = P p 
    where p (Stream [])                       = []
          p (Stream (x:xs)) | x `elem` digits = [(x, Stream xs)]
                            | otherwise       = []

-- FP2.2
between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = (\x y z -> y) <$> a <*> b <*> c

whiteParser :: Parser Char
whiteParser = char '\t' <|> char '\n' <|> char ' '

whitespace :: Parser a -> Parser a
whitespace parser = (many whiteParser *> parser) <* many whiteParser

-- FP2.3
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (\x y -> [x]++y) <$> p <*> some (s *> p)

sep :: Parser a -> Parser b -> Parser [a]
sep p s = (\x y -> [x]++y) <$> p <*> many (s *> p)


-- TODO: Ask a TA to check it
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- FP2.4
---------------------------------
string :: String -> Parser String
string str = P p 
    where p (Stream [])                     = []
          p (Stream xs)     | inList str xs = [(str, (Stream (drop (length str) xs)))]
                            | otherwise     = []

    -- Helper function for string, checks if a string is at the front of a list of characters.
inList :: String -> [Char] -> Bool
inList [] _ = True
inList (s:ss) (x:xs)
                      | s == x      = inList ss xs
                      | otherwise   = False
---------------------------------
identifier :: Parser String 
identifier = whitespace ((\x y -> [x]++y) <$> letter <*> (many (letter <|> dig)))
-- identifier = whitespace (some (letter <|> dig))
---------------------------------
integer :: Parser Integer
integer = (\x -> read x :: Integer) <$> whitespace (some (dig))

symbol :: String -> Parser String
symbol str = whitespace (string str)
---------------------------------
parens :: Parser a -> Parser a 
parens p = (char '(' *> p) <* char ')'

braces :: Parser a -> Parser a 
braces p = (char '[' *> p) <* char ']'

-- TODO Ask a TA about efficiency 