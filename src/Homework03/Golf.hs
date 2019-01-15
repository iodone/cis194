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


localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_,_] = []
localMaxima (x:y:z:zs)
    | (y > x && y > z) = y : (localMaxima rest)
    | otherwise = localMaxima (rest)
    where rest = y:z:zs



histogram :: [Integer] -> String
histogram xs = 
    let 
        frequences = trans xs
        height = foldr1 max frequences 
        visualized = [['0'..'9']] ++ [(replicate 10 '=')] ++ map (\index -> [if e >= index  then '*' else ' ' | e <- frequences]) [1..height]
    in
        unlines (reverse visualized)
        
trans :: [Integer] -> [Integer]
trans xs = map (\i -> (toInteger (length [x | x <- xs, x == i]))) [0..9]