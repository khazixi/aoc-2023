import Data.Char (isDigit)
import Data.List (find, findIndices, foldr, groupBy, nub, nubBy, sortOn)
import Data.Maybe (isJust, isNothing)
import Distribution.Simple (UserHooks (postInst))
import GHC.IO.IOMode (IOMode (ReadMode))
import System.IO (hGetContents, openFile)

findSymbols :: [Char] -> [Int]
findSymbols = findIndices (\c -> not (isDigit c) && c /= '.')

findSymbols2 :: [Char] -> [Int]
findSymbols2 = findIndices (== '*')

findNums :: [Char] -> [Int]
findNums = findIndices isDigit

assignRows :: [String] -> [(Int, String)]
assignRows = zip [0 ..]

findSymbolRow :: [String] -> [(Int, Int)]
findSymbolRow row = concatMap (\d -> zip (repeat $ fst d) (findSymbols $ snd d)) $ assignRows row

-- findSymbolRowP2 :: [String] -> [(Int, Int)]
findSymbolRowP2 row = concatMap (\d -> zip (repeat $ fst d) (findSymbols2 $ snd d)) $ assignRows row

findNumRow :: [String] -> [(Int, Int)]
findNumRow row = concatMap (\d -> zip (repeat $ fst d) (findNums $ snd d)) $ assignRows row

genValid :: (Int, Int) -> [(Int, Int)]
genValid point =
  let actions :: [(Int, Int)] = [(1, 0), (-1, 0), (0, 1), (0, -1), (1, 1), (-1, -1), (1, -1), (-1, 1)]
   in filter (\x -> fst x >= 0 && snd x >= 0) $ map (\action -> (fst action + fst point, snd action + snd point)) actions

filterValid :: (Int, Int) -> [(Int, Int)] -> Bool
filterValid point symbols =
  any (\x -> isJust $ find (== x) symbols) $ genValid point

filterValid2 point symbols =
  filter (\x -> isJust $ find (== x) symbols) $ genValid point

processMonad x symbols =
  case find (== x) symbols of
    Just y -> even $ length y
    Nothing -> False

constructValid :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
constructValid point numbers arr = do
  let row = filter (\x -> fst x == fst point) numbers

  let actions :: [(Int, Int)] = [(0, 1), (0, -1)]
  let a = map (\action -> (fst action + fst point, snd action + snd point)) actions

  let res = filter (\x -> elem x numbers && notElem x arr) a
  let nres = point : res

  case res of
    [] -> point : arr
    _ -> arr ++ concatMap (\r -> constructValid r numbers nres) res

getAdjacent p n = nub . sortOn snd $ constructValid p n []

-- filterByGear :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
filterByGear gp np = do
  let a = groupBy (\x y -> filterValid2 y np == filterValid2 x np) gp
  -- concat . filter (\x -> even $ length x) $ map (nubBy(\x y -> fst x == fst y) ) a
  -- even . length $ head a
  -- a
  map (\x -> filterValid2 x gp) np

constructNumber :: [String] -> [(Int, Int)] -> String
constructNumber g n = do
  let a = map (\t -> (g !! fst t) !! snd t) n
  a

constructNumbers :: [String] -> [[(Int, Int)]] -> [String]
constructNumbers graph tups = do
  let res = map (constructNumber graph) tups
  res

readToInt :: String -> Int
readToInt = read

-- calculate :: [(Int, Int)] -> [(Int, Int)]
calculate lst = do
  let t = zip [0 ..] lst
  let e = map snd $ filter (even . fst) t
  let o = map snd $ filter (odd . fst) t
  sum $ zipWith (*) e o

main = do
  handler <- openFile "2.txt" ReadMode
  contents <- hGetContents handler

  let parsed = words contents
  let syms = findSymbolRow parsed
  let syms2 = findSymbolRowP2 parsed
  let nums = findNumRow parsed

  let valid = filter (`filterValid` syms) nums
  let valid2 = filter (`filterValid` syms2) nums

  let a =
        sum
          . map readToInt
          . filter (not . null)
          . constructNumbers parsed
          . nub
          $ map (`getAdjacent` nums) valid

  let b =
        sum
          . map (product . map readToInt)
          . filter (\x -> length x == 2)
          . map (constructNumbers parsed . nub . map (`getAdjacent` nums))
          $ filterByGear valid2 syms2

  let c = nub . map (`getAdjacent` nums) . concat . filter (\x -> length x == 2) $ filterByGear valid2 syms2
  let d = filterByGear valid2 syms2

  print b

-- print d
