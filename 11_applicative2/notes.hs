{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Applicative hiding (ZipList, (*>))
import Data.List
import Prelude hiding ((*>), sequenceA)

(.+) :: (Applicative f, Num a) => f a -> f a -> f a
(.+) = liftA2 (+)

(.*) :: (Applicative f, Num a) => f a -> f a -> f a
(.*) = liftA2 (*)

n :: [Integer]
n = ([4,5] .* pure 2) .+ [6,1]

m1 :: Maybe Integer
m1 = (pure 3 .+ Just 5) .* Just 8

m2 :: Maybe Integer
m2 = (pure 3 .+ Nothing) .* Just 8

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show, Functor)

instance Applicative ZipList where
  pure = ZipList . repeat
  ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

{-
instance Functor ((->) e) where
  fmap = (.)

instance Applicative ((->) e) where
  pure = const
  f <*> x = \e -> f e (x e)
-}

pair :: Applicative f => f a -> f b -> f (a, b)
--pair fa fb = (,) <$> fa <*> fb
pair = liftA2 (,)

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (\_ b -> b)

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA = foldl' (liftA2 (\xs a -> xs ++ [a])) (pure [])
-- sequenceA = foldr (liftA2 (:)) (pure []) -- order may be important, so this one is not quite right...

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f = sequenceA . map f

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA num = sequenceA . replicate num
