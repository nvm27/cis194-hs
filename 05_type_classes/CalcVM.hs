{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module CalcVM where

import Parser
import StackVM

-- exercise 5

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Program where
  lit x   = [PushI x]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
