{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

import Data.List

-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : [a + b | (a,b) <- zip fibs2 (tail fibs2)]

fibs2' :: [Integer]
fibs2' = 0 : 1 : map op [2..]
  where op idx = fibs2' !! (idx-1) + fibs2' !! (idx-2)

-- exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

showStream :: Show a => Int -> Stream a -> String
showStream n s = "[" ++ representation s ++ "...]"
    where representation = intercalate "," . map show . take n . streamToList

instance Show a => Show (Stream a) where
  show = showStream 20

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat v = Stream v (streamRepeat v)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = Stream v (streamFromSeed f (f v))

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ruler using interleaveStream
-- really not optimal...
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) (Stream y ys) = Stream x $ Stream y $ interleaveStreams xs ys

streamHead :: Stream a -> a
streamHead (Stream x _) = x

streamTail :: Stream a -> Stream a
streamTail (Stream _ xs) = xs

streamDiagonal :: Stream (Stream a) -> Stream a
streamDiagonal (Stream x xs) = Stream (streamHead x) (streamDiagonal (streamMap streamTail xs))

partialRuler :: Integer -> Stream Integer
partialRuler idx = foldr1 interleaveStreams $ map streamRepeat [0..idx]

ruler :: Stream Integer
ruler = streamDiagonal $ streamMap partialRuler nats

-- exercise 6

x :: Stream Integer
x = Stream 0 $ Stream 1 $ streamRepeat 0

scalarMul :: Integer -> Stream Integer -> Stream Integer
scalarMul k = streamMap (*k)

scalarDiv :: Stream Integer -> Integer -> Stream Integer
scalarDiv s k = streamMap (`div` k) s

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  negate (Stream x xs) = Stream (negate x) (negate xs)
  (Stream x xs) + (Stream y ys) = Stream (x + y) (xs + ys)
  (Stream x xs) * b@(Stream y ys) = Stream (x * y) tail'
      where tail' = (x `scalarMul` ys) + (xs * b)

instance Fractional (Stream Integer) where
  (Stream x xs) / (Stream y ys) = result'
      where result' = Stream (x `div` y) ((xs - result' * ys) `scalarDiv` y)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- exercise 7
data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a b c d) = "[[" ++ show a ++ ", " ++ show b ++ "],[" ++ show c ++ "," ++ show d ++ "]]"

instance Num Matrix where
  fromInteger n = Matrix n 0 0 n
  negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
  (Matrix a b c d) + (Matrix e f g h) = Matrix (a+e) (b+f) (c+g) (d+h)
  (Matrix a b c d) * (Matrix e f g h) = Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

fibBase :: Matrix
fibBase = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 n = result
    where Matrix _ result _ _ = fibBase ^ n

fibs4 :: Stream Integer
fibs4 = streamMap fib4 nats
