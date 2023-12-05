{-# LANGUAGE OverloadedStrings #-}

import Data.Map qualified as M
import Data.Text qualified as T
import Data.List (elemIndex, stripPrefix, foldl')
import Data.Maybe (fromJust)

data StrangeMap = M {tables :: [LookupTable]}
  deriving (Show)

data CachedStrangeMap = CM {cache :: M.Map Int Int}
  deriving (Show)

data LookupTable = Table {srcStart :: Int, dstStart :: Int, range :: Int}
  deriving (Show)

parseLookupTable :: String -> LookupTable
parseLookupTable str = Table {srcStart = nums !! 1, dstStart = head nums, range = nums !! 2}
  where
    nums = map read (words str)

parseStrangeMap :: [String] -> StrangeMap
parseStrangeMap str = M {tables = map parseLookupTable (drop 1 str)}

toCachedMap :: StrangeMap -> CachedStrangeMap
toCachedMap (M tables) = foldl' cachingHelper (CM {cache = M.empty}) tables
  where
    cachingHelper :: CachedStrangeMap -> LookupTable -> CachedStrangeMap
    cachingHelper cm (Table start dst range) = CM {cache = M.union (cache cm) (M.fromList (zip [start..(start + range - 1)] [dst..(dst + range - 1)]))}

tableValue :: LookupTable -> Int -> Maybe Int
tableValue t@(Table start dest _) n = if tableContainsValue t n then (Just (dest + (n - start))) else Nothing

tableContainsValue :: LookupTable -> Int -> Bool
tableContainsValue (Table start _ r) n = n >= start && n <= (start + (r - 1))

lookupValue :: StrangeMap -> Int -> Int
lookupValue (M []) n = n
lookupValue (M (curTable:rest)) n = case tableValue curTable n of
  (Just value) -> value
  Nothing -> lookupValue (M rest) n

cachedLookupValue :: CachedStrangeMap -> Int -> Int
cachedLookupValue (CM cache) n = M.findWithDefault n n cache

splitChunks :: [String] -> [[String]]
splitChunks [] = [[]]
splitChunks str = case elemIndex "" str of
  (Just index) -> (take index str) : splitChunks (drop (index + 1) str)
  Nothing -> [str]

readSeeds :: String -> [Int]
readSeeds str = map read (words (fromJust (stripPrefix "seeds: " str)))

locationsOfSeeds :: [StrangeMap] -> [Int] -> [Int]
locationsOfSeeds maps seeds = map (\seed -> foldl' (\val m -> lookupValue m val) seed maps) seeds

cachedLocationsOfSeeds :: [CachedStrangeMap] -> [Int] -> [Int]
cachedLocationsOfSeeds maps seeds = map (\seed -> foldl' (\val m -> cachedLookupValue m val) seed maps) seeds

minLocation :: [StrangeMap] -> [Int] -> Int
minLocation maps seeds = foldl' min maxBound (locationsOfSeeds maps seeds)

cachedMinLocation :: [CachedStrangeMap] -> [Int] -> Int
cachedMinLocation maps seeds = foldl' min maxBound (cachedLocationsOfSeeds maps seeds)

pairs :: [a] -> [(a, a)]
pairs (x:y:xs) = (x, y) : pairs xs
pairs _ = []

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let chunks = splitChunks input

  let maps = map parseStrangeMap (drop 1 chunks)

  let seeds = readSeeds ((head . head) chunks)

  let locations = locationsOfSeeds maps seeds

  -- Part 1
  print (foldl min maxBound locations)

  -- let cachedMaps = map toCachedMap maps

  let seedRanges = map (\(start, range) -> take range [start..]) (pairs seeds)

  -- Currently takes ~1.5 hours to run
  print (foldl' min maxBound (map (minLocation maps) seedRanges))
