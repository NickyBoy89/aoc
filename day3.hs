{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)

data DigitRange = Range {start :: Int, end :: Int, value :: Int}
  deriving (Show)

-- Whether the current digit is adjacent to any symbols
rangeValid :: DigitRange -> [Int] -> Bool
rangeValid _ [] = False
rangeValid r@(Range start end _) (symIndex : rest)
  -- If any symbol is within the range, then the digit is valid
  | symIndex >= (start - 1) && symIndex <= (end + 1) = True
  -- Keep checking
  | otherwise = rangeValid r rest

rangesValid :: [DigitRange] -> [Int] -> [DigitRange]
rangesValid ranges symbols = filter (\r -> rangeValid r symbols) ranges

sumOfRanges :: [DigitRange] -> Int
sumOfRanges [] = 0
sumOfRanges ((Range _ _ val) : rest) = val + sumOfRanges rest

-- Returns a list of all the ranges where digits exist
digitRanges :: String -> [DigitRange]
digitRanges str = digitHelper str 0
  where
    digitHelper :: String -> Int -> [DigitRange]
    digitHelper [] _ = []
    digitHelper orig@(x : xs) index
      | isDigit x = Range {start = index, end = end, value = read found} : digitHelper (drop (length found) orig) (index + length found)
      | otherwise = digitHelper xs (index + 1)
      where
        found = takeWhile isDigit orig
        end = index + (length found - 1)

-- Returns a list of all the indexes of the symbols
extractSymbols :: String -> [Int]
extractSymbols s = indexSymb s 0
  where
    indexSymb :: String -> Int -> [Int]
    indexSymb [] _ = []
    indexSymb ('.' : xs) i = indexSymb xs (i + 1)
    indexSymb (x : xs) i
      | not (isDigit x) = i : indexSymb xs (i + 1)
      | otherwise = indexSymb xs (i + 1)

symbolsForIndex :: Int -> [[Int]] -> [Int]
-- For the first index, take the first two rows
symbolsForIndex 0 symbols = concat (take 2 symbols)
symbolsForIndex index symbols
  | index == (length symbols - 1) = (symbols !! (index - 1)) ++ (symbols !! index)
  | otherwise = (symbols !! (index - 1)) ++ (symbols !! index) ++ (symbols !! (index + 1))

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let ranges = map digitRanges input
  let symbols = map extractSymbols input

  -- print (head input)
  -- print (split isDigit (pack (head input)))
  -- print (split isDigit (pack ("..123.")))
  -- print (digitRanges (head input))
  -- print (map digitRanges input)
  --
  let validSymbols = map (\x -> symbolsForIndex x symbols) (enumFromTo 0 (length symbols - 1))
  print validSymbols
  print ranges

  let validRanges = map (\x -> rangesValid (fst x) (snd x)) (zip ranges validSymbols)

  print (sum (map sumOfRanges validRanges))
