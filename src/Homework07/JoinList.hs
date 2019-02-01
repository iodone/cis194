module Homeowork07.JoinList where

import Data.Monoid
import Homework07.Sized

-- Ex.1
data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

getJoinListM :: Monoid m => JoinList m a -> m
getJoinListM Empty = mempty 
getJoinListM (Single m _) = m 
getJoinListM (Append m _ _) = m

tag :: Monoid m => JoinList m a -> m
tag = getJoinListM

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append ((getJoinListM l) <> (getJoinListM r)) (l, r)


-- Ex.2
getMSize :: (Sized m, Monoid m) => JoinList m a -> Int
getMSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i node | i >= getMSize node = Nothing
index i _ | i < 0 = Nothing
indexJ _ (Single x y) = Just y
indexJ i (Append _ l r) 
    | i < lSize = indexJ i l
    | otherwise = indexJ (i-lSize) r
        where lSize = getMSize l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i node 
    | i >= getMSize node = Empty 
    | i <= 0 = node
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty 
dropJ i (Append _ l r)
    | i < lSize = (drop i l) +++ r
    | otherwise = drop lSize r
        where lSize = getMSize l