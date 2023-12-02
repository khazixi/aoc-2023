{-# LANGUAGE OverloadedStrings #-}
import System.IO (openFile, IOMode (..), hGetContents)
import Data.Text (splitOn, Text, pack, words, lines, split, drop, chunksOf, strip, unpack, isSuffixOf, dropWhile)
import Data.List (transpose)
import Prelude hiding (words, lines, drop, dropWhile)

map2d = map . map

stringToInt :: Text -> Int
stringToInt = read . unpack

parseLine text =
  map strip $ splitOn "," text

parseScore lst str
  | "red" `isSuffixOf` str =  zipWith (+) [stringToInt . head $ words str, 0, 0] lst
  | "blue" `isSuffixOf` str =  zipWith (+) [0, stringToInt . head $ words str, 0] lst
  | "green" `isSuffixOf` str = zipWith (+) [0, 0, stringToInt . head $ words str] lst
  | otherwise = lst

converter = foldl parseScore [0, 0, 0] . parseLine

parser = map2d converter . map2d strip . map (splitOn ";" . dropWhile (/= ' ') . drop 5) . lines . pack

checkList lst
  | head lst > 12 = False
  | head (tail lst) > 14 = False
  | last lst > 13 = False
  | otherwise = True

main = do
  handler <- openFile "2.txt" ReadMode
  contents <- hGetContents handler
  let a = parser contents

  -- Part 1
  let part1 = sum . map fst . filter ( all checkList . snd ) . zip [1..] $ parser contents

  -- Part 2
  let part2 = sum . map product . map2d maximum . map transpose $ parser contents

  print part1
  print part2


