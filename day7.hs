import Data.Set qualified as S
import Data.Map qualified as M

data Hand = Hand {cards :: [Card], bid :: Int}
  deriving (Show)

parseHand :: String -> Hand
parseHand s = Hand {cards = map parseCard (head parts), bid = read (parts !! 1)}
  where
    parts = words s

instance Eq Hand where
  (==) :: Hand -> Hand -> Bool
  (==) (Hand c1 _) (Hand c2 _) = c1 == c2

data Card = Two | Three | Four | Five | Six
          | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
    deriving (Enum, Eq, Show)

parseCard :: Char -> Card
parseCard s = case s of
  '2' -> Two
  '3' -> Three
  '4' -> Four
  '5' -> Five
  '6' -> Six
  '7' -> Seven
  '8' -> Eight
  '9' -> Nine
  'T' -> Ten
  'J' -> Jack
  'Q' -> Queen
  'K' -> King
  'A' -> Ace
  _ -> error ("Unknown value was parsed as a card: " ++ show s)


data HandValue = FiveKind | FourKind | FullHouse | ThreeKind | TwoPair | OnePair | HighCard
  deriving (Eq, Ord)

valueOfHand :: Hand -> HandValue
valueOfHand (Hand cards _) = case () of _
      | length cardSet == 1 -> FiveKind
      | fst (head cardSet) == 4 -> FourKind
      | fst (head cardSet) == 3 && fst (cardSet !! 1) == 2 -> FullHouse
      | fst (head cardSet) == 3 && length cardSet == 3 -> ThreeKind
      | fst (head cardSet) == 2 && fst (cardSet !! 2) == 2 -> TwoPair
      | fst (head cardSet) == 2 -> OnePair
      | otherwise = HighCard
    where
      cardSet = sort (map swap (M.toList (M.fromListWith (+) . map (,1) cards)))

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1@(Hand c1 _) h2@(Hand c2 _) = case compare (valueOfHand h1) (valueOfHand h2) of
    EQ -> find (/=EQ) (map compare (zip c1 c2))
    cmp -> cmp

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let hands = map parseHand input

  print hands
