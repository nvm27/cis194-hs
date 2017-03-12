module Golf where

import Data.Char
import Data.List

-- not really golfing, just solving

-- BEGIN skips --
nthElement :: Int -> [a] -> Maybe (a, [a])
nthElement n xs
  | n <= length xs = Just (xs !! (n-1), drop n xs)
  | otherwise      = Nothing

eachNthElement :: Int -> [a] -> [a]
eachNthElement n = unfoldr $ nthElement n

skips :: [a] -> [[a]]
skips xs = map (`eachNthElement` xs) [1..length xs]
-- END skips --

-- BEGIN localMaxima --
findMaxima :: [Integer] -> [Integer] -> [Integer]
findMaxima (a:b:c:_) xs
  | a < b && b > c = b:xs
  | otherwise      = xs
findMaxima _ xs    = xs

localMaxima :: [Integer] -> [Integer]
localMaxima xs = foldr findMaxima [] (tails xs)
-- END localMaxima --

-- BEGIN histogram --
horizontalLine :: [Integer] -> Int -> String
horizontalLine xs n = digit : '=' : replicate count '*'
  where count = length (elemIndices (toInteger n) xs)
        digit = intToDigit n

normalizeLength :: Int -> String -> String
normalizeLength n s
  | len < n   = s ++ replicate (n - len) ' '
  | otherwise = s
  where len = length s

horizontalHistogram :: [Integer] -> [String]
horizontalHistogram xs = map (normalizeLength maxLength) withoutSpaces
  where withoutSpaces = map (horizontalLine xs) [0..9]
        maxLength = maximum $ map length withoutSpaces

histogram :: [Integer] -> String
histogram xs = unlines $ transpose $ map reverse (horizontalHistogram xs)
-- END histogram --
