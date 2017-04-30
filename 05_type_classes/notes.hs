{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

data Foo = F Int | G Char

instance Eq Foo where
  (F x) == (F y) = x == y
  (G x) == (G y) = x == y
  _ == _         = False

data Foo' = F' Int | G' Char
  deriving (Eq, Ord, Show)

class Blerg a b where
  blerg :: a -> b -> Bool

class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True = [1]
  toList False = [0]

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node v l r) = toList l ++ [v] ++ toList r

sumL :: Listable t => t -> Int
sumL x = sum (toList x)

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y
