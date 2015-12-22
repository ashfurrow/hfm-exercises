oddNumbers :: Int -> [Int]
oddNumbers maxNumber = [1, 3..maxNumber]


sort2 :: Ord a => a -> a -> (a, a)
sort2 x y = if x <= y then (x,y) else (y,x)

sort2' :: Ord a => a -> a -> (a, a)
sort2' x y | x <= y    = (x,y) 
           | otherwise = (y,x)


thrd :: (a, b, c) -> c
thrd (_, _, x) = x

almostEqual (x1, y1) (x2, y2)
  | (x1 == x2) && (y1 == y2) = True
  | (x1 == y2) && (y1 == x2) = True
  | otherwise                = False

isLower :: Char -> Bool
isLower x 
  | elem x ['a'..'z'] = True
  | otherwise = False

mangle :: String -> String
mangle (x:xs) = xs ++ [x]
mangle [] = ""

divide :: Int -> Int -> Int
divide x y = length (multiples y x)
  where
    multiples x max = [x,x+x..max]
