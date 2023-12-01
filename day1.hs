import Data.Char
import Data.List
import Data.Maybe

getDigits :: String -> [Int]
getDigits [] = []
getDigits (x : xs)
  | isDigit x = digitToInt x : getDigits xs
  | otherwise = getDigits xs

getDigits2 :: String -> [Int]
getDigits2 [] = []
getDigits2 line
  | "one" `isPrefixOf` line = 1 : getDigits2 (tail line)
  | "two" `isPrefixOf` line = 2 : getDigits2 (tail line)
  | "three" `isPrefixOf` line = 3 : getDigits2 (tail line)
  | "four" `isPrefixOf` line = 4 : getDigits2 (tail line)
  | "five" `isPrefixOf` line = 5 : getDigits2 (tail line)
  | "six" `isPrefixOf` line = 6 : getDigits2 (tail line)
  | "seven" `isPrefixOf` line = 7 : getDigits2 (tail line)
  | "eight" `isPrefixOf` line = 8 : getDigits2 (tail line)
  | "nine" `isPrefixOf` line = 9 : getDigits2 (tail line)
getDigits2 (x : xs)
  | isDigit x = digitToInt x : getDigits2 xs
  | otherwise = getDigits2 xs

getFirstLast :: [Int] -> Int
getFirstLast lst = head lst * 10 + last lst

main :: IO ()
main = do
  input <- fmap lines (readFile "input")

  let nums = map getDigits input
  let numsWithWords = map getDigits2 input

  let calibSum = sum (map getFirstLast nums)
  let calibSum2 = sum (map getFirstLast numsWithWords)

  print calibSum
  print calibSum2
