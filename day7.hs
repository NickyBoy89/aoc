import Data.List
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Set qualified as S
import Data.Tuple (swap)
import GHC.Utils.Misc (count)

data Hand = Hand {cards :: [Card], bid :: Int}
  deriving (Show)

parseHand :: String -> Hand
parseHand s = Hand {cards = map parseCard (head parts), bid = read (parts !! 1)}
  where
    parts = words s

data Hand' = Hand' {cards' :: [Card'], bid' :: Int}
  deriving (Show)

parseHand' :: String -> Hand'
parseHand' s = Hand' {cards' = map parseCard' (head parts), bid' = read (parts !! 1)}
  where
    parts = words s

instance Eq Hand' where
  (==) :: Hand' -> Hand' -> Bool
  (==) (Hand' c1 _) (Hand' c2 _) = c1 == c2

instance Eq Hand where
  (==) :: Hand -> Hand -> Bool
  (==) (Hand c1 _) (Hand c2 _) = c1 == c2

data Card
  = Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Jack
  | Queen
  | King
  | Ace
  deriving (Enum, Eq, Show, Ord)

data Card'
  = Joker
  | Two'
  | Three'
  | Four'
  | Five'
  | Six'
  | Seven'
  | Eight'
  | Nine'
  | Ten'
  | Queen'
  | King'
  | Ace'
  deriving (Enum, Eq, Show, Ord)

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

parseCard' :: Char -> Card'
parseCard' s = case s of
  '2' -> Two'
  '3' -> Three'
  '4' -> Four'
  '5' -> Five'
  '6' -> Six'
  '7' -> Seven'
  '8' -> Eight'
  '9' -> Nine'
  'T' -> Ten'
  'J' -> Joker
  'Q' -> Queen'
  'K' -> King'
  'A' -> Ace'
  _ -> error ("Unknown value was parsed as a card: " ++ show s)

data HandValue = FiveKind | FourKind | FullHouse | ThreeKind | TwoPair | OnePair | HighCard
  deriving (Eq, Ord, Show, Enum)

valueOfHand :: Hand -> HandValue
valueOfHand (Hand cards _) = case () of
  _
    | length cardSet == 1 -> FiveKind
    | fst (head cardSet) == 4 -> FourKind
    | fst (head cardSet) == 3 && fst (cardSet !! 1) == 2 -> FullHouse
    | fst (head cardSet) == 3 && length cardSet == 3 -> ThreeKind
    | fst (head cardSet) == 2 && fst (cardSet !! 1) == 2 -> TwoPair
    | fst (head cardSet) == 2 -> OnePair
    | otherwise -> HighCard
  where
    cardSet = reverse (sort (map swap (M.toList ((M.fromListWith (+) . map (,1)) cards))))

-- NOTE: This still doesn't work because of some edge cases with Jokers being the dominant type
-- We shouldn't just remove them indiscriminantly
valueOfHand' :: Hand' -> HandValue
valueOfHand' (Hand' cards _) = case () of
  _
    | length cardSet == 1 -> FiveKind
    | fst (head cardSet) == 4 -> toEnum (fromEnum FourKind - min jokers 1)
    | fst (head cardSet) == 3 && fst (cardSet !! 1) == 2 -> toEnum (fromEnum FullHouse - min jokers 2)
    -- With three kind, we can change one to get to four kind
    | fst (head cardSet) == 3 && length cardSet == 3 -> toEnum (fromEnum ThreeKind - min (if jokers > 0 then jokers + 1 else 0) 2)
    -- With two pair, we can get to full house with one card
    | fst (head cardSet) == 2 && fst (cardSet !! 1) == 2 -> toEnum (fromEnum TwoPair - min (if jokers > 0 then jokers + 1 else 0) 3)
    | fst (head cardSet) == 2 -> toEnum (fromEnum OnePair - min jokers 5)
    | otherwise -> toEnum (fromEnum HighCard - min jokers 6)
  where
    removeFirstJoker :: [(Int, Card')] -> [(Int, Card')]
    removeFirstJoker arr@[(_, Joker)] = arr
    removeFirstJoker ((_, Joker) : rest) = rest
    removeFirstJoker arr = arr

    cardSet = removeFirstJoker (reverse (sort (map swap (M.toList ((M.fromListWith (+) . map (,1)) cards)))))

    jokers = count (== Joker) cards

instance Ord Hand where
  compare :: Hand -> Hand -> Ordering
  compare h1@(Hand c1 _) h2@(Hand c2 _) = case compare (valueOfHand h1) (valueOfHand h2) of
    EQ -> fromJust (find (/= EQ) (map (\(x, y) -> compare y x) (zip c1 c2)))
    cmp -> cmp

instance Ord Hand' where
  compare :: Hand' -> Hand' -> Ordering
  compare h1@(Hand' c1 _) h2@(Hand' c2 _) = case compare (valueOfHand' h1) (valueOfHand' h2) of
    EQ -> fromJust (find (/= EQ) (map (\(x, y) -> compare y x) (zip c1 c2)))
    cmp -> cmp

totalScore :: [Hand] -> Int
totalScore hands = sum (zipWith (\hand rank -> bid hand * rank) hands [1 ..])

totalScore' :: [Hand'] -> Int
totalScore' hands = sum (zipWith (\hand rank -> bid' hand * rank) hands [1 ..])

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let hands = reverse (sort (map parseHand input))

  print hands
  print (map valueOfHand hands)

  -- Part 1
  print (totalScore hands)

  let hands' = reverse (sort (map parseHand' input))

  print hands'

  let vals = map valueOfHand' hands'

  print vals

  let jokers = 4
  print (toEnum (fromEnum HighCard - min jokers 6) :: HandValue)

  print (totalScore' hands')
