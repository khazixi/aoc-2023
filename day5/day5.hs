{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import Data.Char (isDigit, isSpace)
import Data.List (find, sort)
import Data.Maybe (fromMaybe)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

parser =
  map
    ( map
        ( map (read @Int . T.unpack)
            . T.words
        )
        . T.lines
        . T.strip
        . T.filter (\c -> isDigit c || isSpace c)
    )
    . filter (not . T.null)
    . T.splitOn ":\n"
    . T.pack

isInRange :: (Ord a) => a -> a -> a -> Bool
isInRange lower upper x = lower <= x && x <= upper

checkRanges :: Int -> [[Int]] -> Int
checkRanges value ranges =
  fromMaybe value 
    . find (/= value) 
    $ map (
      \x -> 
      if isInRange (x !! 1) (x !! 2 + x !! 1 - 1) value 
        then head x - (x !! 1) + value 
      else value
    ) ranges

checkRanges2 :: Int -> [[Int]] -> Int
checkRanges2 value ranges = 
  fromMaybe value 
    . find (/= value) 
    $  map (
      \x -> if isInRange (head x) (x !! 2 + head x - 1) value 
        then (x !! 1) - head x + value 
      else value
    ) ranges

checkInitialRange value range = do
  let selected = take 2 range
  let rest = drop 2 range
  let r = isInRange (head selected) (head selected + last selected) value
  if r
    then Just value
    else case rest of
      [] -> Nothing
      _ -> checkInitialRange value rest

part1 :: [Int] -> [[[Int]]] -> [Int] -> Int
part1 ins ranges arr = do
  let (i : is) = ins
  let a = foldl checkRanges i ranges
  case is of
    [] -> minimum arr
    _ -> part1 is ranges (a : arr)

part2 :: [Int] -> [[[Int]]] -> Int -> Int
part2 ins ranges count = do
  let a = foldl checkRanges2 count ranges
  let b = checkInitialRange a ins
  case b of
    Just v -> count
    Nothing -> part2 ins ranges (count + 1)

main = do
  handle <- openFile "2.txt" ReadMode
  content <- hGetContents handle

  let (instructions : ranges) = parser content

  let vals = concat instructions

  print "-----ANSWERS----"
  print $ part1 vals ranges []
  print $ part2 vals (reverse ranges) 0
