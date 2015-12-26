data Day
  = Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  deriving (Enum, Eq, Show)
  
isWeekday :: Day -> Bool
isWeekday day = not $ elem day [Saturday, Sunday]

-- Exercises
-- 1
map1 :: (a -> b) -> [a] -> [b]
map1 f [] = []
map1 f (x : xs) = f x : map f xs

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = case xs of
  (x:xs) -> (f x) : map2 f xs
  [] -> []

-- 2
nextDay :: Day -> Day
nextDay day = case day of 
  { Sunday -> Monday
  ; Monday -> Tuesday
  ; Tuesday -> Wednesday
  ; Wednesday -> Thursday
  ; Thursday -> Friday
  ; Friday -> Saturday
  ; Saturday -> Sunday
  }
  
-- 3 Cards!

data Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs
  deriving (Show, Eq, Enum)
  
data Face
  = Ace
  | Jack
  | Queen
  | King
  deriving (Show, Eq, Enum)
  
faceValue :: Face -> Int
faceValue c = case c of
  Ace -> 1
  Jack -> 11
  Queen -> 12
  King -> 13


data Card
  = FaceCard 
    { face :: Face
    , suit :: Suit
    }
  | Card 
    { value :: Int
    , suit :: Suit
    }
  deriving (Show, Eq)
  
type Hand = [Card]

cardValue :: Card -> Int 
cardValue card@(Card value _) = value
cardValue card@(FaceCard face _) = faceValue face

handValue :: Hand -> Int
handValue = foldl (\ acc c -> acc + cardValue c) 0


--data PictureObject 
--  = Path    [Point]                   Colour LineStyle 
--  | Circle  Point   Float             Colour LineStyle FillStyle 
--  | Ellipse Point   Float Float Float Colour LineStyle FillStyle 
--  | Polygon [Point]                   Colour LineStyle FillStyle
--  deriving (Show, Eq)










-- 


data Point = Point Float Float
           deriving (Show, Eq)
           
zeroPoint :: Point
zeroPoint = Point 0 0


data Vector = Vector Float Float
            deriving (Show, Eq)

movePointN :: Float -> Vector -> Point -> Point
movePointN n (Vector vx vy) (Point x y) 
  = Point (n * vx + x) (n * vy + y)
  
data Colour = Colour Int Int Int Int  -- red, green, blue, and opacity component
            deriving (Show, Eq)
            
red :: Colour
red = Colour 255 0 0 255
