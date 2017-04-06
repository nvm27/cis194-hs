import Prelude hiding (foldr, foldl, foldl')

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc []     = acc
foldr f acc (x:xs) = x `f` foldr f acc xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc []     = acc
foldl f acc (x:xs) = let acc' = acc `f` x
                     in foldl f acc' xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc []     = acc
foldl' f acc (x:xs) = let acc' = acc `f` x
                      in seq acc' $ foldl' f acc' xs

(?) :: Int -> Int -> Int
_ ? 0 = 0
x ? y = x*y

list :: [Int]
list = [2, 3, undefined, 5, 0]

okey = foldl (?) 1 list
boom = foldl' (?) 1 list

reverse' :: [a] -> [a]
reverse' = foldl' (flip (:)) []

-- foldr for infinite lists
infinite = [1..]

muchInfinite :: [Integer]
muchInfinite = let operator v xs = (2*v):xs
               in foldr operator undefined infinite

if' :: Bool -> a -> a -> a
if' True  v _ = v
if' False _ v = v
