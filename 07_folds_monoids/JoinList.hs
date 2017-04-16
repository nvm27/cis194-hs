{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Buffer
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a `mappend` tag b) a b

-- exercise 2.1

jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a)   = [a]
jlToList (Append _ a b) = jlToList a ++ jlToList b

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty     = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ a)
  | i == 0    = Just a
  | otherwise = Nothing
indexJ i (Append _ a b)
  | lsize > i = indexJ i a
  | otherwise = indexJ (i - lsize) b
  where lsize = (getSize . size . tag) a

-- exercise 2.2

dropJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
dropJ _ Empty        = Empty
dropJ i v | i <= 0   = v
dropJ i (Single _ _) = Empty
dropJ i (Append m l r)
  | i >= totalSize   = Empty
  | i >= lSize       = dropJ (i - lSize) r
  | otherwise        = Append (tag newL `mappend` tag r) newL r
  where totalSize = (getSize . size) m
        lSize     = (getSize . size . tag) l
        newL      = dropJ i l

-- exercise 2.3

takeJ :: (Sized m, Monoid m) => Int -> JoinList m a -> JoinList m a
takeJ _ Empty          = Empty
takeJ i _ | i <= 0     = Empty
takeJ i s@(Single _ _) = s
takeJ i a@(Append m l r)
  | i >= totalSize     = a
  | i >= lSize         = Append (tag l `mappend` tag newR) l newR
  | otherwise          = takeJ i l
  where totalSize = (getSize . size) m
        lSize     = (getSize . size . tag) l
        newR      = takeJ (i - lSize) r

-- exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- exercise 4

scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = foldr1 (+++) . map scoreSizeLine . lines
  line = indexJ
  numLines = getSize . snd . tag
  value = getScore . fst . tag

  replaceLine n s b
    | n < 0          = b
    | n >= oldSize   = b
    | otherwise      = takeJ n b +++ newLine +++ dropJ (n+1) b
    where oldSize = (getSize . size . tag) b
          newLine = scoreSizeLine s

initialBuffer :: JoinList (Score, Size) String
initialBuffer = fromString $ unlines
  [ "This buffer is for notes you don't want to save, and for"
  , "evaluation of steam valve coefficients."
  , "To load a different file, type the character L followed"
  , "by the name of the file."
  ]

main = runEditor editor initialBuffer
