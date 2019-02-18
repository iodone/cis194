module Homework08.Party where


import Homework08.Employee

-- Ex.1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = fun}) (GL es f) = GL (e:es) (f+fun) 
-- glCons e (GL es f) = GL (e:es) (empFun e + fun) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Ex.2
-- use applicate functor to fold [] wrapper
foldTree :: (a -> [b] -> b) -> Tree a -> b 
foldTree f (Node r xs) =  f r (foldTree f <$> xs)