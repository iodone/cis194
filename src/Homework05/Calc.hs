module Homework05.Calc where

import Homework05.ExprT 
import Homework05.Parser


-- Ex.1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Mul x y) = (eval x) * (eval y)
eval (Add x y) = (eval x) + (eval y)

-- Ex.2
evalStr :: String -> Maybe Integer
evalStr xs = eval <$> (parseExp Lit Add Mul xs)

-- Ex.3
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit x = Lit x
    mul x y = Mul x y
    add x y = Add x y

reify :: ExprT -> ExprT
reify  = id

-- Ex.4
instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x = x > 0
    add x y = x || y
    mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    -- lit same as pure, to wrap Integer
    lit = MinMax 
    add (MinMax x) (MinMax y) = lit $ max x y 
    mul (MinMax x) (MinMax y) = lit $ min x y 

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit x = Mod7 $ x `mod` 7
    -- add (Mod7 x) (Mod7 y) = Mod7 $ ((x+y) `mod` 7)
    -- mul (Mod7 x) (Mod7 y) = Mod7 $ ((x*y) `mod` 7)
    add (Mod7 x) (Mod7 y) = lit $ x+y
    mul (Mod7 x) (Mod7 y) = lit $ x*y

testExpr :: Expr a => Maybe a
testExpr = parseExp lit add mul "(3 * -4) + 5"