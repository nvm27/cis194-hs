toDigits :: Integer -> [Integer]
toDigits num
  | num <= 0  = []
  | otherwise = toDigits (num `div` 10) ++ [num `mod` 10 ]

toDigitsRev :: Integer -> [Integer]
toDigitsRev num
  | num <= 0  = []
  | otherwise = num `mod` 10 : toDigitsRev (num `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther [x]      = [x]
doubleEveryOther (x:y:zs)
  | length zs `mod` 2 == 0 = 2*x : y : doubleEveryOther zs
  | otherwise              = x : 2*y : doubleEveryOther zs

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs)
  | x < 10    = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate num = checksum num `mod` 10 == 0
  where checksum = sumDigits . doubleEveryOther . toDigits

-- other solutions

doubleEveryOtherForward :: [Integer] -> [Integer]
doubleEveryOtherForward []       = []
doubleEveryOtherForward [x]      = [x]
doubleEveryOtherForward (x:y:zs) = x : 2*y : doubleEveryOtherForward zs

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' = reverse . doubleEveryOtherForward . reverse

doubleEveryOtherLength :: [Integer] -> Int -> [Integer]
doubleEveryOtherLength []  0        = []
doubleEveryOtherLength [x] 1        = [x]
doubleEveryOtherLength (x:y:zs) len
  | len `mod` 2 == 0 = 2*x : y : doubleEveryOtherLength zs (len-2)
  | otherwise        = x : 2*y : doubleEveryOtherLength zs (len-2)

doubleEveryOther'' xs = doubleEveryOtherLength xs (length xs)

validate' :: Integer -> Bool
validate' num = checksum num `mod` 10 == 0
  where checksum = sumDigits . doubleEveryOtherForward . toDigitsRev
