import Data.List

combination :: Integer -> Integer -> Integer
combination i j = i + j + 2*i*j

listToRemove :: Integer -> [Integer]
listToRemove n = [ combination i j | j <- [1..n], i <- [1..j], combination i j <= n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ listToRemove n)
