import Prelude hiding (map, zipWith, filter, foldr, foldl, sum, map)
import Data.Char

average :: Float -> Float -> Float
average a b = (a + b) / 2.0


map :: (a -> b) -> [a] -> [b]
map f []       = []
map f (x : xs) = f x : map f xs


zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op []       _        = []
zipWith op _        []       = []
zipWith op (x : xs) (y : ys) = (x `op` y) : zipWith op xs ys


filter :: (a -> Bool) -> [a] -> [a]
filter p [] 
  = []
filter p (x:xs)
  | p x = x : (filter p xs)
  | otherwise = filter p xs


allSquares :: Num a => [a] -> [a]
allSquares = map (\x -> x * x)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op n []     = n
foldr op n (x:xs) = x `op` foldr op n xs


minList :: [Int] -> Int
minList = foldr min maxBound 

sum :: Num a => [a] -> a
sum = foldr (+) 0 


allEven :: [Int] -> Bool
allEven = foldr (\x b -> even x && b) True

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl op acc []     = acc
foldl op acc (x:xs) = foldl op (acc `op` x) xs

stringToInt :: String -> Int
stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

fastReverse :: [a] -> [a]
fastReverse = foldl (\accList x -> x : accList) []

sumOfSquareRoots :: (Ord a, Floating a) => [a] -> a
sumOfSquareRoots = sum . map sqrt . filter (> 0) 


-- Exercises

-- 1
natSum = sum . enumFromTo 1


-- 2
map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\a b -> (f a) : b) []

-- 4&5 Skipping lol





