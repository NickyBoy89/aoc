{-# LANGUAGE OverloadedStrings #-}

import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T

data ScratchCard = Card {cardNumber :: Int, winningNumbers :: S.Set Int, have :: S.Set Int}
  deriving (Show)

parseCard :: String -> ScratchCard
parseCard s = Card {cardNumber = parseHead cardHead, winningNumbers = S.fromList (parseIntList winning), have = S.fromList (parseIntList (T.drop 1 have))}
  where
    (cardHead, rest) = T.breakOn ":" (T.pack s)
    (winning, have) = T.breakOn "|" (T.drop 1 rest)

    parseHead :: T.Text -> Int
    parseHead str = read (T.unpack (T.words str !! 1))

    textList :: T.Text -> [T.Text]
    textList str = T.words (T.strip str)

    parseIntList :: T.Text -> [Int]
    parseIntList str = map read (toStrings (textList str))

    toStrings = map T.unpack

-- CardCounts is a mapping between a card's number and its count
type CardCounts = M.Map Int Int

initialCounts :: Int -> CardCounts
initialCounts numCards = M.fromList (zip (enumFromTo 1 (numCards + 1)) (replicate numCards 1))

countOfCard :: CardCounts -> Int -> Int
countOfCard state cardNum = M.findWithDefault 0 cardNum state

updateCardCount :: CardCounts -> Int -> Int -> CardCounts
updateCardCount state cardNum cardCount = M.insert cardNum cardCount state

incrementCardCount :: CardCounts -> Int -> CardCounts
incrementCardCount state cardNum = updateCardCount state cardNum (countOfCard state cardNum + 1)

updateCounts :: CardCounts -> [Int] -> CardCounts
updateCounts state [] = state
updateCounts state cardNums = foldl incrementCardCount state cardNums

applyN :: CardCounts -> [[Int]] -> CardCounts
applyN state [] = state
applyN state nums = foldl updateCounts state nums

totalCards :: [ScratchCard] -> CardCounts
totalCards cards = cardCount cards (initialCounts (length cards))
  where
    cardCount :: [ScratchCard] -> CardCounts -> CardCounts
    cardCount [] state = state
    cardCount (card : rest) state = cardCount rest (applyN state (replicate currentCardCount (cardsWon card)))
      where
        currentCardCount = countOfCard state (cardNumber card)

cardsWon :: ScratchCard -> [Int]
cardsWon card = enumFromTo (cardNumber card + 1) (cardNumber card + numMatching card)

numMatching :: ScratchCard -> Int
numMatching (Card _ winning have) = length (S.intersection winning have)

cardScore :: ScratchCard -> Int
cardScore card = case numMatching card of
  0 -> 0
  similar -> 2 ^ (similar - 1)

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let cards = map parseCard input

  let scores = map cardScore cards

  -- Part 1
  print (sum scores)

  -- Part 2
  print (sum (M.elems (totalCards cards)))
