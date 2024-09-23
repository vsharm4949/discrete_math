-- Ch 3
copy :: [a] -> [a]
copy (x:xs) = x : copy xs
copy [] = []

inverse :: [(a, b)] -> [(b, a)]
inverse [] = []
inverse ((a,b):xs) = (b, a) : inverse xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) =
  if x < y
    then x : y : merge xs ys
    else y : x : merge xs ys

(!!!) :: [a] -> Int -> Maybe a
(!!!) (x:xs) 0 = Just x
(!!!) [] n = Nothing
(!!!) (x:xs) n = (!!!) xs (n-1)

lookup_ :: Eq a => [(a, b)] -> a -> Maybe b
lookup_ [] k = Nothing
lookup_ ((f, s):xs) k = 
  if f == k
    then Just s
    else lookup_ xs k

count :: Eq a => [a] -> a -> Int
count [] k = 0
count (x:xs) k = if x == k then 1 + count xs k else count xs k

remove_ :: Eq a => [a] -> a -> [a]
remove_ [] k = []
remove_ (x:xs) k = if x == k then remove_ xs k else x:(remove_ xs k)

removeAlternate :: [a] -> [a]
removeAlternate [] = []
removeAlternate (x:xs) = _removeAlternateHelper xs

_removeAlternateHelper :: [a] -> [a]
_removeAlternateHelper [] = []
_removeAlternateHelper (x:xs) = x : (removeAlternate xs)

extract :: [Maybe a] -> [a]
extract [] = []
extract (Nothing:xs) = extract xs
extract (Just x:xs) = x : (extract xs)

_findHelper :: String -> String -> Int -> Maybe Int
_findHelper [] s2 n = Nothing
_findHelper (x:xs) s2 n 
  = if length s2 > length (x:xs)
      then Nothing
      else if take (length s2) (x:xs) == s2
        then Just n
        else _findHelper xs s2 (n+1)

find :: String -> String -> Maybe Int
find s1 s2 = _findHelper s1 s2 0
