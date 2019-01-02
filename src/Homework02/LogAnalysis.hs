module Homework02.LogAnalysis where

import Homework02.Log

-- Ex. 1
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
        

-- Ex. 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert x Leaf = Node Leaf x Leaf
insert x (Node left rv right) = 
    let xTimestamp = getLogMessageTimestamp x
        rvTimestamp = getLogMessageTimestamp rv
    in 
        if xTimestamp < rvTimestamp 
        then Node (insert x left) rv right  
        else Node left rv (insert x right)


getLogMessageTimestamp :: LogMessage -> TimeStamp 
getLogMessageTimestamp (LogMessage _ time _) = time
getLogMessageTimestamp _ = 0

--Ex. 3
build :: [LogMessage] -> MessageTree
build xs = foldl (\tree x -> insert x tree) Leaf xs

--Ex. 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left rv right) =
    (inOrder left) ++ [rv] ++ (inOrder right)

-- Ex. 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMsgs = 
    let sortedMsgs = inOrder . build $ logMsgs
    in
        foldl (\wrongMsgs x -> if isRelevantMessage x then wrongMsgs ++ [(getLogMessageString x)] else wrongMsgs) [] sortedMsgs
        -- foldr (\x wrongMsgs -> if isRelevantMessage x then (getLogMessageString x):wrongMsgs else wrongMsgs) [] sortedMsgs


getLogMessageString :: LogMessage -> String
getLogMessageString (LogMessage _ _ msg) = msg
getLogMessageString _ = ""

isRelevantMessage :: LogMessage -> Bool
isRelevantMessage (LogMessage mType _ _) = 
    case mType of 
        (Error level) -> if level >= 50 then True else False
        _ -> False
isRelevantMessage _ = False