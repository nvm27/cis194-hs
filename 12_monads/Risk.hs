{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

-- exercise 2

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

getDieValues :: Int -> Rand StdGen [DieValue]
getDieValues n = replicateM n die

data MatchupResult = Attacked | Defended
  deriving (Eq, Show)

matchup :: DieValue -> DieValue -> MatchupResult
matchup a d | a > d = Attacked | otherwise = Defended

fullMatchup :: [DieValue] -> [DieValue] -> (Int, Int)
fullMatchup as ds         = (failedAttacks result, successfulAttacks result)
  where result            = zipWith matchup (sortDesc as) (sortDesc ds)
        successfulAttacks = length . filter (== Attacked)
        failedAttacks     = length . filter (== Defended)

adjustBattlefield :: (Int, Int) -> Battlefield -> Battlefield
adjustBattlefield (a, d) bf = Battlefield (attackers bf - a) (defenders bf - d)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = attacks >>= \as ->
            defends >>= \ds ->
            return (result as ds bf)
  where attackUnits = min 3 (attackers bf - 1)
        defendUnits = min 2 (defenders bf)
        attacks = getDieValues attackUnits
        defends = getDieValues defendUnits
        result as ds = adjustBattlefield (fullMatchup as ds)

-- exercise 3

shouldStop :: Battlefield -> Bool
shouldStop (Battlefield a d) = a < 2 || d < 1

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | shouldStop bf = return bf
  | otherwise     = battle bf >>= invade

-- exercise 4

data InvasionResult = Destroyed | Survived
  deriving (Eq, Show)

getInvasionResult :: Battlefield -> InvasionResult
getInvasionResult (Battlefield a d)
  | a < 2     = Survived
  | d < 1     = Destroyed
  | otherwise = undefined

invade' :: Battlefield -> Rand StdGen InvasionResult
invade' = fmap getInvasionResult . invade

successProb' :: Int -> Battlefield -> Rand StdGen Double
successProb' n bf = samples >>= \s ->
                    return (result s)
  where samples  = replicateM n (invade' bf)
        result s = (genericLength . filter (== Destroyed)) s / genericLength s

successProb :: Battlefield -> Rand StdGen Double
successProb = successProb' 1000
