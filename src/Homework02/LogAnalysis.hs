module Homework02.LogAnalysis where

import Homework02.Log

-- log string to LogMessage AST
parseMessage :: String -> LogMessage
parseMessage message = 
    case tokens of 
        ("I":timestamp:rest) -> LogMessage Info (read timestamp) (unwords' rest)
        ("W":timestamp:rest) -> LogMessage Warning (read timestamp) (unwords' rest)
        ("E":level:timestamp:rest) -> LogMessage (Error $ read level) (read timestamp) (unwords' rest)
        _ -> Unknown $ unwords' tokens
    where tokens = words' message

words' :: String -> [String]
words' s = case dropWhile (== ' ') s of
            "" -> []
            s' -> w:words(xs)
                where  (w, xs) = break (== ' ') s' 

unwords' :: [String] -> String
unwords' [] = ""
unwords' ws = foldr1 (\w acc -> w ++ ' ':acc) ws


parse :: String -> [LogMessage]
-- parse s = parseMessage <$> (lines $ s)
parse = (map parseMessage) . lines
        





