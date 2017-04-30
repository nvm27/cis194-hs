{-# OPTIONS_GHC -Wall #-}

data Funny f a = Funny a (f a)

x :: Maybe Int
x = Just 5

y :: Maybe Int
y = fmap (*2) x
