{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party where

import Employee

import Data.List
import Data.Tree

-- exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  (GL xs f1) `mappend` (GL ys f2) = GL (xs ++ ys) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v ts) = f v $ map (treeFold f) ts

-- exercise 3

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e ls = (glCons e withoutBoss, withBoss)
  where summed = mconcat ls
        withBoss = fst summed
        withoutBoss = snd summed

-- exercise 4

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- exercise 5

stringToEmpTree :: String -> Tree Employee
stringToEmpTree = read

prepareOutput :: GuestList -> [String]
prepareOutput (GL employees fun) = totalFun : nameList
  where totalFun = "Total fun: " ++ show fun
        nameList = (sort . map empName) employees

main :: IO ()
main = readFile "company.txt" >>= (putStr . unlines . prepareOutput . maxFun . stringToEmpTree)
