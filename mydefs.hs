y = x + 1
x = 2 * 3 :: Int

is_a :: Char -> Bool
is_a 'a' = True
is_a c = False

is_hello :: String -> Bool
is_hello ('h':'e':'l':'l':'o':[]) = True
is_hello (x:xs) = False

strip_one_front :: String -> String
strip_one_front (x:xs) = if x == ' ' then xs else x:xs

list_sum :: [Int] -> Int
list_sum [] = 0
list_sum (x:xs) = x + list_sum(xs)

toBool :: Int -> Bool
toBool 0 = False
toBool 1 = True
convert :: [Int] -> [Bool]
convert xs = map toBool xs

is0 :: Char -> Bool
is0 '0' = True
is0 x = False
member0 :: String -> Bool
member0 x = or $ map is0 x 

addJust :: (Maybe Int) -> (Maybe Int) -> (Maybe Int)
addJust Nothing Nothing = Nothing
addJust (Just x) Nothing = Just x
addJust Nothing (Just y) = Just y
addJust (Just x) (Just y) = Just (x + y)

addJusts :: [Maybe Int] -> [Maybe Int] -> [Maybe Int]
addJusts xs ys = zipWith addJust xs ys

data Metal = Iron | Bronze | Silver | Gold | Platinum
    deriving (Eq, Show)

transmute :: Metal -> Metal
transmute Iron = Bronze
transmute Bronze = Silver
transmute Silver = Gold
transmute Gold = Platinum
transmute Platinum = Iron

data CoinPile = Penny Int | Nickel Int | Dime Int | Quarter Int
    deriving Show
