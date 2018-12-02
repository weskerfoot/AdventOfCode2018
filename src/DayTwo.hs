module DayTwo
    ( calculate
    ) where

import Data.List (group, sort)

hasLength n = any (stringHasLength n)

stringHasLength n xs = any (== n) $ map length xs

count n = filter (stringHasLength n) . (map (group . sort))

getBoxIDs = do
  input <- readFile "2.txt"
  return $ lines input

calculate = do
  boxes <- getBoxIDs
  let n1 = length $ count 2 boxes
  let n2 = length $ count 3 boxes
  print $ n1 * n2
