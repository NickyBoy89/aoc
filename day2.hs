{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Text (pack, split, strip, unpack)

data Round = Round {red :: Int, green :: Int, blue :: Int}
  deriving (Show)

data Game = Game {gameId :: Int, rounds :: [Round]}
  deriving (Show)

idOfGame :: Game -> Int
idOfGame (Game id _) = id

parseRound :: String -> Round
parseRound str = foldl mergeSubrounds (Round 0 0 0) (map parseInstance (allStrings (split (== ',') (pack str))))
  where
    parseInstance :: String -> Round
    parseInstance inst = case words inst of
      [num, color] -> case color of
        "red" -> Round {red = read num, green = 0, blue = 0}
        "green" -> Round {red = 0, green = read num, blue = 0}
        "blue" -> Round {red = 0, green = 0, blue = read num}
        other -> error ("Unknown color to parse: " ++ show other)
      parts -> error ("Unknown number of parts in intance: " ++ show parts)

    allStrings = map unpack

    mergeSubrounds :: Round -> Round -> Round
    mergeSubrounds (Round r1 g1 b1) (Round r2 g2 b2) = Round {red = r1 + r2, green = g1 + g2, blue = b1 + b2}

extractGameId :: String -> (Int, String)
extractGameId str = (read (drop 5 (fst (parts str))) :: Int, drop 1 (unpack (strip (pack (snd (parts str))))))
  where
    parts s = splitAt (fromJust (elemIndex ':' s)) s

splitRounds :: String -> [String]
splitRounds str = map unpack (map strip (split (== ';') (pack str)))

strToGame :: String -> Game
strToGame str = constructGame (fst (extractGameId str)) (map parseRound (splitRounds (snd (extractGameId str))))
  where
    constructGame :: Int -> [Round] -> Game
    constructGame id r = Game {gameId = id, rounds = r}

isGameValid :: Round -> Game -> Bool
isGameValid comparison (Game _ rounds) = areRoundsValid comparison rounds
  where
    areRoundsValid :: Round -> [Round] -> Bool
    areRoundsValid _ [] = True
    areRoundsValid r (cur : rest)
      | isRoundValid r cur = areRoundsValid r rest
      | otherwise = False

    isRoundValid :: Round -> Round -> Bool
    isRoundValid (Round r1 g1 b1) (Round r2 g2 b2)
      | r2 > r1 || g2 > g1 || b2 > b1 = False
      | otherwise = True

powerOfGame :: Game -> Int
powerOfGame (Game _ rounds@(start : _)) = powerOfRound (foldl minRound start rounds)
  where
    powerOfRound :: Round -> Int
    powerOfRound (Round 0 g b) = g * b
    powerOfRound (Round r 0 b) = r * b
    powerOfRound (Round r g 0) = r * g
    powerOfRound (Round r g b) = r * g * b

    minRound :: Round -> Round -> Round
    minRound (Round r1 g1 b1) (Round r2 g2 b2) = Round (max r1 r2) (max g1 g2) (max b1 b2)
powerOfGame (Game _ _) = error "Game with no rounds"

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let games = map strToGame input

  let comparison = Round 12 13 14

  let validGames = filter (isGameValid comparison) games

  print (sum (map idOfGame validGames))

  print (sum (map powerOfGame games))
