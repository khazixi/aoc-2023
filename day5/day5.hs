{-# LANGUAGE OverloadedStrings #-}
import System.IO (openFile, IOMode (ReadMode), hGetContents)
import qualified Data.Text as T
import Data.Char (isDigit)
import qualified Data.Char as T
import Data.List (sort)


parser content = 
  map (sort)
  . map (
    concatMap (
      map (read @Int . T.unpack) 
      . T.words
    )
    . T.lines 
    . T.strip 
    . T.filter (\c -> isDigit c || T.isSpace c)
  )
  . filter (not . T.null)
  . T.splitOn ":\n"
  $ T.pack content

main = do
  handle <- openFile "1.txt" ReadMode
  content <- hGetContents handle

  let parsed = parser content

  print content
  print "------------"
  print parsed
