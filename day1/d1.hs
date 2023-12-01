import Data.Array (Ix (inRange))
import Data.ByteString (hGetContents, split)
import Data.ByteString.Char8 (unpack)
import Data.Char (isNumber)
import Data.List (isPrefixOf, tails)
import System.IO (IOMode (ReadMode), openFile)

parse = words . unpack

stripContents :: String -> String
stripContents = filter (inRange ('0', '9'))

stripUnused :: String -> String
stripUnused str = [head str, last str]

duplicate :: String -> String
duplicate str
  | length str == 1 = str ++ str
  | otherwise = str

travel lst res
  | null lst = res
  | "one" `isPrefixOf` lst = travel  (tail lst) (res ++ ['1'])
  | "two" `isPrefixOf` lst = travel  (tail lst) (res ++ ['2'])
  | "three" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['3'])
  | "four" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['4'])
  | "five" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['5'])
  | "six" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['6'])
  | "seven" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['7'])
  | "eight" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['8'])
  | "nine" `isPrefixOf`  lst = travel  (tail lst) (res ++ ['9'])
  | isNumber . head $ lst  = travel (tail lst) $ res ++ [head lst]
  | otherwise = travel (tail lst) res

getNums x =
  stripUnused . travel x $ []

stringToInt :: String -> Integer
stringToInt = read

main :: IO ()
main = do
  fhandle <- openFile "2" ReadMode
  contents <- hGetContents fhandle
  let usable = parse contents
  let res = map (duplicate . stripUnused . stripContents) usable

  let a = sum . map (stringToInt . duplicate . stripUnused . stripContents) $ res

  print a
  -- Part 1

  f2handle <- openFile "2" ReadMode
  contents <- hGetContents f2handle
  let use = parse contents

  -- print . tails . head $ use
  let v = sum . map(stringToInt . getNums) $ use
  print v
