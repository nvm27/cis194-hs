computeXOR :: Bool -> Bool -> Bool
computeXOR False False = False
computeXOR False True  = True
computeXOR True False  = True
computeXOR True True   = False

xor :: [Bool] -> Bool
xor = foldr computeXOR False

mapper :: (a -> b) -> a -> [b] -> [b]
mapper f x ys = f x : ys

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (mapper f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc lst = foldr (flip f) acc (reverse lst)
