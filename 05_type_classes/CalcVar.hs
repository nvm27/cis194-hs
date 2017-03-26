{-# LANGUAGE FlexibleInstances #-}

import qualified Data.Map as M

-- exercise 6

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit v _ = Just v
  add x y m = (+) <$> x m <*> y m
  mul x y m = (*) <$> x m <*> y m

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
