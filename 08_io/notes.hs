-- main = putStrLn "Hello, Haskell!"
-- main = putStrLn "Hello" >> putStrLn "world!"
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> print (n+1)))

data D = C {
  field1 :: Int,
  field2 :: Integer,
  field3 :: String
}
