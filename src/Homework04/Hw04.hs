module Homework04.Hw04 where

import Data.List ((\\))

-- Ex.1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (+(-2)) . filter (even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter (even) . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- Ex.2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
foldTree :: (Show a, Eq a) => [a] -> Tree a
foldTree = foldr insert Leaf

insert :: (Show a, Eq a) => a -> Tree a -> Tree a
insert r Leaf = Node 0 Leaf r Leaf
insert r (Node _ left v right) = Node height left' v right'
    where
        (inserted, _) = orderByHeightAsc left right
        newTree = insert r inserted  
        -- TODO: For optimization when left height equal right height, swap them
        (left', right') = if inserted == left then (newTree, right) else (left, newTree)
        height = succ $ maxTreeHeight left' right'

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

maxTreeHeight :: Tree a -> Tree a -> Integer
maxTreeHeight xt yt = max (treeHeight xt) (treeHeight yt)

orderByHeightAsc :: Tree a -> Tree a -> (Tree a, Tree a) 
orderByHeightAsc Leaf t = (Leaf, t)
orderByHeightAsc t Leaf = (Leaf, t)
orderByHeightAsc t0@(Node h0 _ _ _) t1@(Node h1 _ _ _) = if h0 > h1 then (t1, t0) else (t0, t1)
    

-- Ex.3.1
xor :: [Bool] -> Bool
-- xor = foldr (/=) False
xor = foldr it False
    where it x acc 
            | x == False = acc
            | otherwise = not acc
        
-- Ex.3.2
map' :: (a -> b) -> [a] -> [b]
-- map' f = fst . foldr (\x (acc, m) -> ((m x):acc, f)) ([],f)
-- map' = foldr (:).f []
map' f = foldr (\x acc -> f x:acc) []

-- Ex.3.3
sieveSundaram :: Integer -> [Integer]
-- TODO: map id . ((||) []) what's mean ? why ???????
sieveSundaram n = map ((+1).(*2)) . ((\\) [1..n]) $ [i+j+2*i*j | i <- [1..n], j <- [1..i], i+j+2*i*j <= n]