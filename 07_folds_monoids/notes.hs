{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l v r) = f (treeFold e f l) v (treeFold e f r)

treeSize :: Tree a -> Integer
treeSize = treeFold 0 (\l _ r -> 1 + l + r)

treeSum :: Tree Integer -> Integer
treeSum = treeFold 0 (\l v r -> l + v + r)

treeDepth :: Tree a -> Integer
treeDepth = treeFold 0 (\l _ r -> 1 + max l r)

flatten :: Tree a -> [a]
flatten = treeFold [] (\l v r -> l ++ [v] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l v r -> l `max` v `max` r)

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving Show

exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)   = f i
exprTFold f g h (Add a b) = exprTFold f g h a `g` exprTFold f g h b
exprTFold f g h (Mul a b) = exprTFold f g h a `h` exprTFold f g h b

eval :: ExprT -> Integer
eval = exprTFold id (+) (*)

numLiterals :: ExprT -> Integer
numLiterals = exprTFold (const 1) (+) (+)

-- instance Monoid [a] where
--   mempty  = []
--   mappend = (++)

newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)

newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid (Product a) where
  mempty  = Product 1
  mappend = (*)

prod :: [Integer] -> Integer
prod = getProduct . mconcat . map Product

-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--   mempty = (mempty, mempty)
--   mappend (a,b) (c,d) = (mappend a c, mappend b d)


