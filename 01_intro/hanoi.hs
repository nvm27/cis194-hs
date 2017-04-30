{-# OPTIONS_GHC -Wall #-}

import Data.List

type Peg'  = String
type Move' = (Peg', Peg')

hanoi' :: Integer -> Peg' -> Peg' -> Peg' -> [Move']
hanoi' 0 _ _ _ = []
hanoi' n a b c = hanoi' (n-1) a c b ++ [(a, b)] ++ hanoi' (n-1) c b a

-- better Hanoi + tester

data Peg = A | B | C
type Move = (Peg, Peg)
type State = ([Integer], [Integer], [Integer])

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a, b)] ++ hanoi (n-1) c b a

makeInput :: Integer -> [Integer]
makeInput size = [1..size]

executeMove :: State -> Move -> State
executeMove (x:xs, ys, zs) (A, B) = (xs, x:ys, zs)
executeMove (x:xs, ys, zs) (A, C) = (xs, ys, x:zs)
executeMove (xs, y:ys, zs) (B, C) = (xs, ys, y:zs)
executeMove (xs, y:ys, zs) (B, A) = (y:xs, ys, zs)
executeMove (xs, ys, z:zs) (C, A) = (z:xs, ys, zs)
executeMove (xs, ys, z:zs) (C, B) = (xs, z:ys, zs)

executeAllMoves :: State -> [Move] -> State
executeAllMoves = foldl executeMove

solve :: [Integer] -> State
solve input = executeAllMoves (input, [], []) $ hanoi (genericLength input) A B C

solveSize :: Integer -> State
solveSize size = solve $ makeInput size
