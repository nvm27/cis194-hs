-- some haskell comment

{-
- some strange
- haskell comment
- because why not
-}

{- y :: Int
y = 7 -}

l :: [Int]
l = 1:l

biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

m :: Integer
m = 2^(2^(2^(2^2)))

x, y :: Int
x = 5 `mod` 2
y = 5 `div` 2

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n
  | n > 0 = n + sumtorial (n-1)
  | otherwise = 0

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise = 3*n + 1

isEven :: Integer -> Bool
isEven n = n `mod` 2 == 0

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x+y

hello :: String
-- hello = 'h' : 'e' : 'l' : 'l' : 'o' : []
hello = ['h', 'e', 'l', 'l', 'o']

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

maxHailstone :: Integer -> Integer
maxHailstone n = maximum $ hailstoneSeq n

intLength :: [Integer] -> Integer
intLength [] = 0
intLength (x:xs) = intLength xs + 1

sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x:y:zs) = (x+y) : sumEveryTwo zs
