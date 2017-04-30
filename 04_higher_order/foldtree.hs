{-# OPTIONS_GHC -Wall #-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

nodeDepth :: Tree a -> Integer
nodeDepth Leaf           = -1
nodeDepth (Node d _ _ _) = d

newDepth :: Integer -> Integer -> Integer
newDepth old child
    | child < old = old
    | otherwise   = child + 1

balancedInsert :: a -> Tree a -> Tree a
balancedInsert v Leaf    = Node 0 Leaf v Leaf
balancedInsert v (Node d l c r)
    | l_depth <= r_depth = Node (newDepth d new_l_depth) new_l c r
    | otherwise          = Node (newDepth d new_r_depth) l c new_r
  where l_depth     = nodeDepth l
        r_depth     = nodeDepth r
        new_l       = balancedInsert v l
        new_r       = balancedInsert v r
        new_l_depth = nodeDepth new_l
        new_r_depth = nodeDepth new_r

foldTree :: [a] -> Tree a
foldTree = foldr balancedInsert Leaf
