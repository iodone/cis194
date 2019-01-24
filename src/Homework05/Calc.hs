{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Homework05.Calc where

import Homework05.ExprT 
import Homework05.Parser
import qualified Homework05.StackVM as VM
import qualified Data.Map as M 


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

-- Ex.5
instance Expr VM.Program where
   lit x = [VM.PushI x] 
   add x y = y ++ x ++ [VM.Add] 
   mul x y = y ++ x ++ [VM.Mul] 


compile :: String -> Maybe VM.Program
compile = parseExp lit add mul 

testVM = VM.stackVM <$> compile "(3 * 5) + 4"

-- Ex.6

class HasVars a where
    var :: String -> a

data VarExprT = Lit' Integer
            | Add' VarExprT VarExprT
            | Mul' VarExprT VarExprT
            | Var String
            deriving (Show, Eq)

instance HasVars VarExprT where
    var = Var 

instance Expr VarExprT where
    lit = Lit'
    add = Add'
    mul = Mul'

mapHasVars :: String -> M.Map String Integer -> Maybe Integer
-- mapHasVars m s = M.lookup m s
mapHasVars = M.lookup

instance HasVars (M.Map String Integer -> Maybe Integer) where
    -- var  = mapHasVars 
    -- var s m
    var s m = M.lookup s m


instance Expr (M.Map String Integer -> Maybe Integer) where
    -- use partial application， because add type is (a -> a -> M.Map Sring Interger -> Maybe Integer), so when add appliy fx fy, add fx fy type is (M.map String Integer -> Maybe Integer)
    -- lit x = (\_ -> Just x)
    lit x _ = Just x
    -- add fx fy = (\m -> (+) <$> fx m <*> fy m)
    add fx fy m = (+) <$> fx m <*> fy m
    -- mul fx fy = (\m -> (*) <$> fx m <*> fy m)
    mul fx fy m = (*) <$> fx m <*> fy m 

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs expr = expr $ M.fromList vs