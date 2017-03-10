{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

{-
-- getTokenInternal :: (Delimiter) -> (Token being built) -> (Rest of string) -> (Token, Rest)
getTokenInternal :: Char -> String -> String -> (String, String)
getTokenInternal _ xs "" = (xs, "")
getTokenInternal d xs (y:ys)
  | y == d  = (xs, ys)
  | otherwise = getTokenInternal d (xs ++ [y]) ys

-- getToken :: Char -> Input -> (Token, Rest)
getToken :: Char -> String -> (String, String)
getToken d = getTokenInternal d []

split :: Char -> String -> [String]
split d s = case getToken d s of
  (x, "") -> [x]
  (x, y)   -> x : split d y

join :: [String] -> String
join []     = ""
join [x]    = x
join (x:xs) = x ++ " " ++ join xs
-}

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : time : msg)        -> LogMessage Info (read time) (unwords msg)
  ("W" : time : msg)        -> LogMessage Warning (read time) (unwords msg)
  ("E" : code : time : msg) -> LogMessage (Error (read code)) (read time) (unwords msg)
  msg                       -> Unknown (unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert m@LogMessage{} Leaf = Node Leaf m Leaf
insert m@(LogMessage _ time _) (Node l c@(LogMessage _ ctime _) r)
  | time < ctime = Node (insert m l) c r
  | otherwise    = Node l c (insert m r)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l c r) = inOrder l ++ [c] ++ inOrder r

isImportant :: LogMessage -> Bool
isImportant (LogMessage (Error code) _ _) = code >= 50
isImportant _                             = False

extractMessage :: LogMessage -> String
extractMessage (LogMessage Info _ msg)    = msg
extractMessage (LogMessage Warning _ msg) = msg
extractMessage (LogMessage Error{} _ msg) = msg
extractMessage (Unknown msg)   = msg

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . inOrder . build . filter isImportant

write = do
  contents <- testWhatWentWrong parse whatWentWrong "error.log"
  writeFile "output.txt" (unlines contents)
