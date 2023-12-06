data RaceRecord = Race {duration :: Int, record :: Int}
  deriving (Show)

parseRecords :: [String] -> [String] -> [RaceRecord]
parseRecords [] _ = []
parseRecords _ [] = []
parseRecords (time : times) (record : records) = Race {duration = read time, record = read record} : parseRecords times records

travel :: Int -> Int -> Int
travel timeLimit timeHeld = timeHeld * timeRemaining
  where
    timeRemaining = timeLimit - timeHeld

numberWins :: RaceRecord -> Int
numberWins (Race maxTime record) = length (filter (> record) (map (travel maxTime) [0 .. maxTime]))

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let times = drop 1 (words (head input))
  let records = drop 1 (words (input !! 1))

  let races = parseRecords times records

  -- Part 1
  print (product (map numberWins races))

  let combinedTime = concat times
  let combinedRecords = concat records

  let race = Race {duration = read combinedTime, record = read combinedRecords}

  -- Part 2
  print (numberWins race)
