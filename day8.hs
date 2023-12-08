import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.List (foldl')
import GHC.Utils.Misc (countWhile)

data Elem = Elem String
  deriving (Eq, Ord, Show)

endsWithChar :: Char -> Elem -> Bool
endsWithChar c (Elem e) = e !! 2 == c

data Map = Map {instrs :: [Char], nodes :: M.Map Elem (Elem, Elem)}
  deriving (Show)

parseMap :: [String] -> Map
parseMap lns = Map {instrs = head lns, nodes = M.fromList (map parseMapLine (drop 2 lns))}
  where
    parseMapLine :: String -> (Elem, (Elem, Elem))
    parseMapLine str = (Elem (take 3 str), (Elem (take 3 (drop 7 str)), Elem (take 3 (drop 12 str))))

turnsUntilResolution :: Map -> Int
turnsUntilResolution m = turnHelper m (Elem "AAA") (cycle (instrs m)) 0
  where
    turnHelper :: Map -> Elem -> [Char] -> Int -> Int
    turnHelper _ (Elem "ZZZ") _ turn = turn
    turnHelper m state instrs turn = turnHelper m ((case head instrs of 
      'R' -> snd
      'L' -> fst
      _ -> error "Unknown movement character")
      (fromJust (M.lookup state (nodes m)))) (drop 1 instrs) (turn + 1)

turnsUntilResolutionStart :: Map -> Elem -> Int
turnsUntilResolutionStart m e = turnHelper m e (cycle (instrs m)) 0
  where
    turnHelper :: Map -> Elem -> [Char] -> Int -> Int
    turnHelper _ state _ turn | endsWithZ state = turn
    turnHelper m state instrs turn = turnHelper m ((case head instrs of 
      'R' -> snd
      'L' -> fst
      _ -> error "Unknown movement character")
      (fromJust (M.lookup state (nodes m)))) (drop 1 instrs) (turn + 1)

endsWithA = endsWithChar 'A'
endsWithZ = endsWithChar 'Z'

transElem :: Map -> Elem -> Char -> Elem
transElem m state direc = ((case direc of
  'R' -> snd
  'L' -> fst
  _ -> error "Unknown movement character") (fromJust (M.lookup state (nodes m))))

transitionN :: Map -> [Char] -> Elem -> Int -> Elem
transitionN m instrs state times = foldl' (transElem m) state (take times instrs)

ghostTurnsUntilResolution :: Map -> Int
ghostTurnsUntilResolution m = turnHelper m startElems (cycle (instrs m)) 0
  where
    endsWithA = endsWithChar 'A'
    endsWithZ = endsWithChar 'Z'

    startElems = filter endsWithA (M.keys (nodes m))

    elemTransition :: Elem -> Char -> Elem
    elemTransition state direc = ((case direc of
      'R' -> snd
      'L' -> fst
      _ -> error "Unknown movement character") (fromJust (M.lookup state (nodes m))))

    turnHelper :: Map -> [Elem] -> [Char] -> Int -> Int
    turnHelper m state instrs turn = countWhile (not . all . endsWithZ) (scanl (transElem) startElems instrs)
    -- turnHelper m states instrs turn
    --   | all endsWithZ states = turn
    --   | otherwise = turnHelper m (map (\state -> elemTransition state (head instrs)) states) (drop 1 instrs) (turn + 1)

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let inputMap = parseMap input

  -- print (turnsUntilResolution inputMap)
  print (filter endsWithA (M.keys (nodes inputMap)))

  let startNodes = filter endsWithA (M.keys (nodes inputMap))

  print (map (turnsUntilResolutionStart inputMap) startNodes)

  print (ghostTurnsUntilResolution inputMap)
