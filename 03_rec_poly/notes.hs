{-# OPTIONS_GHC -Wall #-}

data List t = E | C t (List t)
  deriving Show

lst :: List Int
lst = C 3 (C 1 (C 4 E))

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

filterList :: (t -> Bool) -> List t -> List t
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

mapList :: (a -> b) -> List a -> List b
mapList _ E = E
mapList f (C x xs) = C (f x) (mapList f xs)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNel :: NonEmptyList a -> a
headNel (NEL x _) = x

tailNel :: NonEmptyList a -> [a]
tailNel (NEL _ xs) = xs
