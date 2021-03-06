{- CIS 194 HW 10
   due Monday, 1 April
-}

module Homework10.AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Ex.1
first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  -- fmap f (Parser g) = (Parser (\r -> first f <$> r) <$> g)
  -- fmap f (Parser g) = (Parser (\xs -> first f <$> (g $ xs))
  -- fmap f (Parser g) = Parser (first f <$> . g)
  -- fmap f (Parser g) = (Parser first f <$> <$> g)
  fmap f (Parser g) = Parser $ fmap (first f) . g


-- Ex.2
instance Applicative Parser where
  pure x = Parser $ (\xs -> Just(x, xs))
  Parser f <*> Parser p = Parser (\xs -> f xs >>= h) 
    where h (fp, es) = first fp <$> p es

type Name = String
data Employee = Emp {name :: Name, phone :: String} deriving (Eq, Show)

parseName :: Parser Name 
parseName = Parser f
  where
    f [] = Nothing
    f xs = Just (head names, unwords $ tail names)
      where names = words xs

parsePhone :: Parser String 
parsePhone = show <$> posInt

-- Ex.3
abParser :: Parser (Char, Char)
-- abParser = (\x y -> (x,y)) <$> satisfy (== 'a') <*> satisfy (== 'b')
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
-- abParser_ = (\_ _ -> ()) <$> satisfy (== 'a') <*> satisfy (== 'b')
abParser_ = () <$ abParser

intPair :: Parser [Integer] 
-- intPair = (\x _ y -> x : y : []) <$> posInt <*> char ' ' <*> posInt
intPair = (\x y -> x : y : []) <$> posInt <* char ' ' <*> posInt

-- Ex.4

instance Alternative Parser where
  empty = Parser $ (\xs -> Nothing)
  -- Parser h <|> Parser g = Parser f where 
  --   f xs 
  --     |  h xs == Nothing = g xs 
  --     |  otherwise h xs
  a <|> b = Parser f where   
    f xs = runParser a xs <|> runParser b xs

-- Ex.5
intOrUppercase :: Parser ()
intOrUppercase = (() <$ posInt) <|>  (() <$ satisfy isUpper)
-- intOrUppercase = (const () <$> posInt) <|>  (const () <$> satisfy isUpper)