module Homework06.Fibonacci where

import Data.List (foldl') 

-- Ex.1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib1 :: [Integer]
fib1 = fib <$> [0..]

-- Ex.2
fib2 :: [Integer]
-- fib2 = foldl' (\acc x -> acc ++ [fst x, snd x]) [] . iterate (\(x,y) -> (x+y, x+2*y)) $ (0,1)
fib2 = map fst . iterate (\(x,y) -> (y, x+y)) $ (0,1)


-- Ex.3
-- match scala stream style
data Stream a = a ::: Stream a
infixr 5 :::

streamToList :: Stream a -> [a]
streamToList (x:::xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show  = ("Streaming is " ++) . (++ " ... for infinite."). show . take 20 . streamToList 

-- define stream must recursion
stream1 :: Stream Integer
stream1 = 1 ::: stream1

stream2 :: Stream Char
stream2 = 'a' ::: stream2

stream3 :: Stream Integer
stream3 = 5 ::: 4 ::: 3 ::: 2 ::: stream3

stream4 :: Stream Integer
stream4 = 9 ::: 8 ::: 7 ::: 6 ::: 5 ::: 4 ::: 3 ::: 2 ::: stream1

-- Ex.4
instance Functor Stream where
    fmap f (h:::t) = f h ::: fmap f t

streamRepeat :: a -> Stream a
streamRepeat x = x ::: (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
-- streamMap f (s:::ss) = f s ::: (streamMap f ss)
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = f x ::: (streamFromSeed f $ f x)

-- Ex.5

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)