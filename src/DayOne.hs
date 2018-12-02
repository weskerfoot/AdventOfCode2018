module DayOne
    ( calculate
    ) where

import qualified Data.HashSet as H
import Control.Applicative ((<$>))
import Data.Function (on)
import Data.List (groupBy)

parseValue :: String -> Integer -> Integer
parseValue x =
  case x of
    ('+':ds) -> (+(read ds))
    ('-':ds) -> (subtract (read ds))

composeIntervals = foldr1 (.)

getIntervals = do
  input <- readFile "1.txt"
  return $ map parseValue $ lines input

findDups acc _ [] = Nothing
findDups acc m (f:fs)
  | H.member (f acc) m = Just (f acc)
  | otherwise = findDups (f acc) (H.insert (f acc) m) fs

partOne = do
  intervals <- getIntervals
  print $ composeIntervals intervals $ 0

partTwo = do
  intervals <- getIntervals
  let repeating = intervals ++ repeating
  print $ findDups (0 :: Integer) (H.empty) repeating


calculateIntervals xs = scanl1 (+) xs

calculate = partOne >> partTwo
