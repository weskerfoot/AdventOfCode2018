module DayOne
    ( calculate
    ) where

import qualified Data.HashSet as M

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
  | M.member (f acc) m = Just (f acc)
  | otherwise = findDups (f acc) (M.insert (f acc) m) fs

partOne = do
  intervals <- getIntervals
  print $ composeIntervals intervals $ 0

partTwo = do
  intervals <- getIntervals
  let repeating = intervals ++ repeating
  print $ findDups (0 :: Integer) M.empty repeating

calculate = partOne >> partTwo
