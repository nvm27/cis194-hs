{-# OPTIONS_GHC -Wall #-}

greaterThan100 :: [Integer] -> [Integer]
-- greaterThan100 = filter (\x -> x > 100)
greaterThan100 = filter (> 100)

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g = f . g
-- comp f g = \x -> f(g x)

schonfinkel :: ((a, b) -> c) -> a -> b -> c
schonfinkel f a b = f (a, b)
-- schonfinkel = curry

unschonfinkel :: (a -> b -> c) -> (a, b) -> c
unschonfinkel f (a, b) = f a b
-- unschonfinkel = uncurry

rFold :: b -> (a -> b -> b) -> [a] -> b
rFold z _ []     = z
rFold z f (x:xs) = f x (rFold z f xs)

lFold :: b -> (b -> a -> b) -> [a] -> b
lFold z f xs = rFold z (flip f) (reverse xs)
