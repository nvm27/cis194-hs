module Calc where

import ExprT
import Parser

-- exercise 1

eval :: ExprT -> Integer
eval (Lit x)     = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- exercise 2

evalStr :: String -> Maybe Integer
evalStr s = case result of
                 Just expr -> Just (eval expr)
                 Nothing -> Nothing
  where result = parseExp Lit Add Mul s

evalStr' :: String -> Maybe Integer
evalStr' s = eval <$> result
  where result = parseExp Lit Add Mul s

-- exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

-- exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit                       = MinMax
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit                   = Mod7
  add (Mod7 x) (Mod7 y) = Mod7 (x + y `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 (x * y `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
