import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)

data Direction = North | South | West | East
  deriving (Show, Eq)

data Tile = NS | WE | NE | NW | SW | SE | Ground | Start
  deriving (Show, Eq)

parseTile :: Char -> Tile
parseTile c = case c of
  '|' -> NS
  '-' -> WE
  'L' -> NE
  'J' -> NW
  '7' -> SW
  'F' -> SE
  '.' -> Ground
  'S' -> Start
  other -> error ("Unknown tile type: " ++ show other)

newtype Maze = Maze {tiles :: M.Map (Int, Int) Tile}
  deriving (Show)

parseMaze :: [String] -> Maze
parseMaze inputs = Maze (parseMazeHelper M.empty inputs 0)
  where
    parseMazeHelper :: M.Map (Int, Int) Tile -> [String] -> Int -> M.Map (Int, Int) Tile
    parseMazeHelper m [] _ = m
    parseMazeHelper m (curLine : rest) rowIndex = M.union mergedTiles (parseMazeHelper mergedTiles rest (rowIndex + 1))
      where
        mergedTiles = M.union m (M.fromList (zip (zip (replicate (length curLine) rowIndex) [0 .. length curLine - 1]) (map parseTile curLine)))

findStart :: Maze -> (Int, Int)
findStart m = fst (head (M.toList (M.filterWithKey (\_ tile -> tile == Start) (tiles m))))

validDirections :: Maze -> (Int, Int) -> [Direction]
validDirections (Maze tiles) pos = case fromJust (M.lookup pos tiles) of
  NS -> [North, South]
  WE -> [West, East]
  NE -> [North, East]
  NW -> [North, West]
  SW -> [South, West]
  SE -> [South, East]
  _ -> error "Invalid directions for position"

tracePosition :: Maze -> (Int, Int) -> [[(Int, Int)]]
tracePosition = undefined
  where
    isStart :: Maze -> (Int, Int) -> Bool
    isStart (Maze t) pos = fromJust (M.lookup pos t) == Start

-- `traceMaze` follows the path in the maze and returns a list of all the
-- positions and their distances from the start
traceMaze :: Maze -> [(Int, (Int, Int))]
traceMaze = undefined

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let maze = parseMaze input

  print (findStart maze)

  print maze
