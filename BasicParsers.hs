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

-- Parses a single letter 
letter :: Parser Char
letter = P p 
    where p (Stream [])                        = []
          p (Stream (x:xs)) | x `elem` letters = [(x, Stream xs)]
                            | otherwise        = []

parsedLetter = runParser letter (Stream "ab12")

-- Parses a single digit 
dig :: Parser Char
dig = P p 
    where p (Stream [])                       = []
          p (Stream (x:xs)) | x `elem` digits = [(x, Stream xs)]
                            | otherwise       = []

parsedDigit = runParser dig (Stream "12ab")

-- FP2.2
-- Takes in three Parsers and only returns the result of the second parser
between :: Parser a -> Parser b -> Parser c -> Parser b
between a b c = (\x y z -> y) <$> a <*> b <*> c

parseMiddle = runParser (between letter dig letter) (Stream "a2an")

-- Helper function which detects tabs newlines or spaces
whiteParser :: Parser Char
whiteParser = char '\t' <|> char '\n' <|> char ' '

-- Takes in a Parser and returns a new one which removes the whitespace
-- before and after the parsed result
whitespace :: Parser a -> Parser a
whitespace parser = (many whiteParser *> parser) <* many whiteParser

parseAWhiteSpace = runParser (whitespace letter) (Stream "  A  b ")

-- FP2.3
-- Takes in two Parsers, the first being the parsed items 
-- and the second being the dilimiter (there will be at least one element)
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = (:) <$> p <*> many (s *> p)

sep1Parse = runParser (sep1 letter (char ',')) (Stream "a,v,s,a")

-- Takes in two Parsers, the first being the parsed items 
-- and the second being the dilimiter (the list can be empty)
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> parseNothing
    where parseNothing = P (\stream -> [([], stream)])

sepParse = runParser (sep letter (char ',')) (Stream "")

-- Tries to apply parser p and upon failure should result in x
option :: a -> Parser a -> Parser a
option x p = p <|> pure x

-- testing for option
testOption = runParser (option 'x' dig) (Stream "abcde")

-- FP2.4
-- Helper function for string, checks if a string is at the front of a list of characters.
inList :: String -> [Char] -> Bool
inList [] _ = True
inList (s:ss) (x:xs)
                      | s == x      = inList ss xs
                      | otherwise   = False

-- Works similiar to char but allows for more than one character
string :: String -> Parser String
string str = P p 
    where p (Stream [])                     = []
          p (Stream xs)     | inList str xs = [(str, (Stream (drop (length str) xs)))]
                            | otherwise     = []

parseString = runParser (string "str") (Stream "str23")

-- Parses an identifier (a string that begins with a letter
-- and may be followed by other letters or digits)
identifier :: Parser String 
identifier = whitespace ((\x y -> [x]++y) <$> letter <*> (many (letter <|> dig)))

identifierParser = runParser (identifier) (Stream "a123")

-- Parses an integer
integer :: Parser Integer
integer = (\x -> read x :: Integer) <$> whitespace (some (dig))

integerParse = runParser integer (Stream "1231")

-- Parses a string and removes the whitespace before and after the parsed element
symbol :: String -> Parser String
symbol str = whitespace (string str)

symbolParser = runParser (symbol "ab") (Stream "    ab     as ")

-- Takes in a Parser and removes the parentheses before and after the parsed result 
parens :: Parser a -> Parser a 
parens p = whitespace ((char '(' *> p) <* char ')')

parensParser = runParser (parens identifier) (Stream "(as12)")

-- Works in a similiar way as parens but removes '{' at the beginning and '}' at the end
braces :: Parser a -> Parser a 
braces p = whitespace ((char '{' *> p) <* char '}')

bracesParser = runParser (braces identifier) (Stream "{as12}")