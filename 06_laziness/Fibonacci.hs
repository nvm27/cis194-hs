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
data Stream a = StreamCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (StreamCons x xs) = x : streamToList xs

showStream :: Show a => Int -> Stream a -> String
showStream n s = "[" ++ representation s ++ "...]"
    where representation = intercalate "," . map show . take n . streamToList

instance Show a => Show (Stream a) where
  show = showStream 20

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat v = StreamCons v (streamRepeat v)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (StreamCons x xs) = StreamCons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f v = StreamCons v (streamFromSeed f (f v))

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- ruler using interleaveStream
-- really not optimal...
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (StreamCons x xs) (StreamCons y ys) = StreamCons x $ StreamCons y $ interleaveStreams xs ys

streamHead :: Stream a -> a
streamHead (StreamCons x _) = x

streamTail :: Stream a -> Stream a
streamTail (StreamCons _ xs) = xs

streamDiagonal :: Stream (Stream a) -> Stream a
streamDiagonal (StreamCons x xs) = StreamCons (streamHead x) (streamDiagonal (streamMap streamTail xs))

partialRuler :: Integer -> Stream Integer
partialRuler idx = foldr1 interleaveStreams $ map streamRepeat [0..idx]

ruler :: Stream Integer
ruler = streamDiagonal $ streamMap partialRuler nats
