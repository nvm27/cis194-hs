{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- exercise 3

module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score s) = s

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

interleaveUpperCase :: String -> String
interleaveUpperCase [] = []
interleaveUpperCase (x:xs) = x : toUpper x : interleaveUpperCase xs

score :: Char -> Score
score x
  | x `elem` interleaveUpperCase "aeilnorstu" = Score 1
  | x `elem` interleaveUpperCase "dg" = Score 2
  | x `elem` interleaveUpperCase "bcmp" = Score 3
  | x `elem` interleaveUpperCase "fhvwy" = Score 4
  | x `elem` interleaveUpperCase "k" = Score 5
  | x `elem` interleaveUpperCase "jx" = Score 8
  | x `elem` interleaveUpperCase "qz" = Score 10
  | otherwise = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
