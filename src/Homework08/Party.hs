{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Homework08.Party where


import Data.Monoid()
import Data.Tree

import Homework08.Employee

-- Ex.1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = fun}) (GL es f) = GL (e:es) (f+fun) 
-- glCons e (GL es f) = GL (e:es) (empFun e + fun) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

instance Semigroup GuestList where
    (GL gla funa) <> (GL glb funb) = GL (gla <> glb) (funa + funb)

instance Monoid GuestList where
    mempty = GL [] 0

-- Ex.2
-- use applicate functor to fold [] wrapper
treeFold :: (a -> [b] -> b) -> Tree a -> b 
treeFold f (Node r xs) =  f r (treeFold f <$> xs)

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e = glCons e . mconcat

-- Ex.3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gls = (withBossL gls, withoutBossL) where
    withoutBossL = mconcat . map snd $ gls
    withBossL = combineGLs e . map fst 

-- Ex.4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel 

-- Ex.5
formatGL :: GuestList -> String
formatGL (GL es fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines (empName <$> es)

process :: IO ()
process = readFile "src/Homework08/company.txt" >>= processGuestList >>= putStr where
    processGuestList = return . formatGL . maxFun . read 

