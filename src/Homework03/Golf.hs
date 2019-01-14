module Homework03.Golf where

skips :: [a] -> [[a]]
skips [] = []
skips xs = (\n -> everyNth n xs) <$> [1..length xs]

everyNth :: Int -> [a] -> [a]
everyNth 0 _ = []
everyNth _ [] = []
everyNth n xs = 
    case rest of 
        [] -> [] 
        (y:ys) -> y : (everyNth n ys)
    where rest = drop (n-1) xs

