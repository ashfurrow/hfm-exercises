import Data.Char
import Prelude hiding (length, enumFromTo)

natSum :: (Num a, Ord a, Show a) => a -> a
natSum 0 = 0
natSum n | n > 0     = n + natSum (n - 1) 
         | otherwise = error ("natSum: Input value " ++ (show n) ++ "is too small!")
         

repeatN :: Int -> a -> [a]
repeatN 0 _ = []
repeatN n x = x : repeatN (n-1) x

suffixes :: String -> [String]
suffixes "" = []
suffixes s = s : suffixes (tail s)


allSquares :: Num a => [a] -> [a]
allSquares [] = []
allSquares (x:xs) = x*x : allSquares xs

allToUpper :: String -> String
allToUpper []                 = []
allToUpper (chr : restString) = toUpper chr : allToUpper restString


extractDigits' :: String -> String
extractDigits' [] = []
extractDigits' (x:xs) | isDigit x = x : (extractDigits xs)
                     | otherwise = extractDigits xs

extractDigits :: String -> String
extractDigits []
  = []
extractDigits (chr : restString)
  | isDigit chr = chr : extractDigits restString
  | otherwise   =       extractDigits restString

x `snoc` xs = xs ++ [x]
reverse' :: [a] -> [a]
reverse' []     = []
--reverse' (x:xs) = x `snoc` reverse' xs
reverse' (x:xs) = reverse' xs ++ [x]


stringToInt :: String -> Int
stringToInt str = stringToIntAcc 0 str
  where
    stringToIntAcc :: Int -> String -> Int
    stringToIntAcc acc []
      = acc
    stringToIntAcc acc (chr : restString) 
      = stringToIntAcc (10 * acc + digitToInt chr) restString



fastReverse :: [a] -> [a]
fastReverse xs = reverseAcc [] xs
  where
    reverseAcc :: [a] -> [a] -> [a]
    reverseAcc accList []     = accList
    reverseAcc accList (x:xs) = reverseAcc (x : accList) xs

-- Exercises
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs


fact :: Int -> Int
fact n | n < 1 = error "Can't call on argument < 1"
fact 1 = 1
fact n = n * fact (n-1)


enumFromTo :: Int -> Int -> [Int]
enumFromTo m n = enumFromToAcc [] m n
  where
    enumFromToAcc :: [Int] -> Int -> Int -> [Int]
    enumFromToAcc acc m n
      | m == n = (m : acc)
      | otherwise = m : (enumFromToAcc acc (m + 1) n)


removeOdd :: [Int] -> [Int]
removeOdd [] = []
removeOdd (x:xs)
  | odd x = removeOdd xs
  | otherwise = x : (removeOdd xs)







