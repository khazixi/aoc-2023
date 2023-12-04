{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Text as T
import qualified Data.Map as M

import System.IO (openFile, IOMode (ReadMode), hGetContents)
import Data.List (intersect)
import Data.Maybe (fromMaybe)

parse :: String -> [[[Int]]]
parse = 
   map (
    map (
          map (read @Int . T.unpack) 
          . T.words 
          . T.strip
        )
    . T.splitOn "|" 
    . T.drop 2 
    . T.dropWhile ( /= ':'))
  . T.lines
  . T.pack

calculateScore :: Int -> Int
calculateScore number 
  | number == 0 = 0
  | otherwise = 2 ^(number - 1)

part1 :: [[[Int]]] -> Int
part1 input = 
  sum
  $ map (\row -> calculateScore . length $ intersect (head row) (row !! 1)) input

part2 :: [[[Int]]] -> M.Map Int Int ->  Int -> Int
part2 (i:is) kv current  
    | null is = M.foldl (+) 1 kv
    | otherwise = do
      let matched = length $ intersect (head i) (i !! 1)
      let keyVal = fromMaybe 0 (M.lookup current kv)
      let nkv = M.insertWith (+) current 1 kv

      let copies = map (, keyVal + 1) [(current + 1) .. (current + matched)]

      let nnkv = M.unionsWith (+) [ nkv, M.fromList copies ]
      part2 is nnkv (current + 1)
  
main = do
  handler <- openFile "2.txt" ReadMode
  contents <- hGetContents handler

  let parsedContents = parse contents 

  print $ part1 parsedContents
  print $ part2 parsedContents (M.singleton 1 0) 1
