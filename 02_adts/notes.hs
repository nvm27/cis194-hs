{-# OPTIONS_GHC -Wall #-}

data Thing = Shoe | Ship | SealingWax | Cabbage | King
  deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, Ship, King, Shoe]

isSmall :: Thing -> Bool
isSmall Shoe       = True
isSmall Ship       = False
isSmall SealingWax = True
isSmall Cabbage    = True
isSmall King       = False

isSmall' :: Thing -> Bool
isSmall' Ship = False
isSmall' King = False
isSmall' _    = True

data FailableDouble = Failure | OK Double
  deriving Show

d01, d02 :: FailableDouble
d01 = Failure
d02 = OK 3.1415

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

data Person =
  Person String Int Thing
  deriving Show

brent :: Person
brent = Person "Brent" 31 SealingWax

getAge :: Person -> Int
getAge (Person _ age _) = age

baz :: Person -> String
-- baz p@(Person _ _ _) = "The person is " ++ show p
baz p@Person{} = "The person is " ++ show p

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                        Failure -> 0
                        OK d    -> d

data IntList = Empty | Cons Int IntList
  deriving Show

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons a l) = a * intListProd l

intList :: IntList
intList = Cons 3 (Cons 1 (Cons 4 Empty))
