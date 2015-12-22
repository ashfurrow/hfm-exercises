
inc :: Num a => a -> a
inc x = x + 1

average :: Float -> Float -> Float
average x y = (x + y) / 2

--(+) :: String -> String -> String
--(+) a b = a ++ b

--square :: Int -> Int
square x = x * x

showResult :: Int -> String
showResult x = "The result is " ++ show x

showAreaOfCircle :: Float -> String
showAreaOfCircle x = "radius of " ++ show x ++ "cm is " ++ show (pi * x * x) ++ "cm^2"
