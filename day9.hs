changes :: [Int] -> [Int]
changes [] = []
changes [_] = []
changes (x : rest) = head rest - x : changes rest

lastHist :: [Int] -> [Int]
lastHist state = lastHistHelper state []
  where
    lastHistHelper :: [Int] -> [Int] -> [Int]
    lastHistHelper state change
      | all (== 0) state = change
      | otherwise = lastHistHelper (changes state) (last state : change)

firstHist :: [Int] -> [Int]
firstHist state = firstHistHelper state []
  where
    firstHistHelper :: [Int] -> [Int] -> [Int]
    firstHistHelper state change
      | all (== 0) state = change
      | otherwise = firstHistHelper (changes state) (head state : change)

resolveFirst :: [Int] -> Int
resolveFirst hist = foldl (flip (-)) 0 hist

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let hists = map (map (read :: String -> Int) . words) input

  let resolvedHists = map lastHist hists

  print (sum (map sum resolvedHists))

  print (sum (map (resolveFirst . firstHist) hists))
