{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isDigit)

data DigitRange = Range {start :: Int, end :: Int, value :: Int}
  deriving (Show)

-- Whether the range is adjacent to the provided value on the x-axis
isAdjacent :: DigitRange -> Int -> Bool
isAdjacent (Range start end _) n = n >= (start - 1) && n <= (end + 1)

-- Whether the range contains the provided value on the x-axis
withinRange :: DigitRange -> Int -> Bool
withinRange (Range start end _) n = n >= start && n <= end

-- Whether the current digit is adjacent to any symbols
rangeValid :: DigitRange -> [Int] -> Bool
rangeValid _ [] = False
rangeValid r symbols = any (isAdjacent r) symbols

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

validForIndex :: Int -> [[a]] -> [a]
-- For the first index, take the first two rows
validForIndex 0 symbols = concat (take 2 symbols)
validForIndex index symbols
  | index == (length symbols - 1) = (symbols !! (index - 1)) ++ (symbols !! index)
  | otherwise = (symbols !! (index - 1)) ++ (symbols !! index) ++ (symbols !! (index + 1))

symbolsForIndex :: Int -> [[Int]] -> [Int]
-- For the first index, take the first two rows
symbolsForIndex 0 symbols = concat (take 2 symbols)
symbolsForIndex index symbols
  | index == (length symbols - 1) = (symbols !! (index - 1)) ++ (symbols !! index)
  | otherwise = (symbols !! (index - 1)) ++ (symbols !! index) ++ (symbols !! (index + 1))

data Schematic = S
  { contents :: [String],
    partNumbers :: [[DigitRange]],
    symbolLocations :: [(Int, Int)]
  }
  deriving (Show)

parseSchematic :: [String] -> Schematic
parseSchematic str =
  S
    { contents = str,
      partNumbers = map digitRanges str,
      symbolLocations = symPos (map extractSymbols str) 0
    }
  where
    symPos :: [[Int]] -> Int -> [(Int, Int)]
    symPos [] _ = []
    symPos (rowSymbols : rest) rowIndex = (zip (replicate (length rowSymbols) rowIndex) rowSymbols) ++ symPos rest (rowIndex + 1)

gearPositions :: Schematic -> [(Int, Int)]
gearPositions s = filter (\(row, col) -> ((contents s !! row) !! col) == '*') (symbolLocations s)

digitAt :: Schematic -> (Int, Int) -> Bool
digitAt s (row, col) = any (`withinRange` col) (partNumbers s !! row)

digitsAround :: Schematic -> (Int, Int) -> [DigitRange]
digitsAround s (row, col) = filter (`isAdjacent` col) (validForIndex row (partNumbers s))

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let ranges = map digitRanges input
  let symbols = map extractSymbols input

  let validSymbols = map (\x -> symbolsForIndex x symbols) (enumFromTo 0 (length symbols - 1))

  let validRanges = map (\x -> rangesValid (fst x) (snd x)) (zip ranges validSymbols)

  -- Part 1
  print (sum (map sumOfRanges validRanges))

  let schematic = parseSchematic input

  let validGears = filter (\x -> length x == 2) (map (digitsAround schematic) (gearPositions schematic))

  print (sum (map (product . map value) validGears))
