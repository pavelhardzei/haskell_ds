-- determine if the element is the part of the list
elemFunc :: Eq t => t -> [t] -> Bool
elemFunc _ [] = False
elemFunc a (x:xs)
    | a == x    = True
    | otherwise = elemFunc a xs


-- remove all duplicates from the list
nubFunc :: Eq a => [a] -> [a]
nubFunc [] = []
nubFunc (x:xs)
    | elemFunc x xs = nubFunc xs
    | otherwise     = x : nubFunc xs


-- is list in ascending order
isAsc :: Ord a => [a] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x1:x2:xs)
    | x1 > x2    = False
    | otherwise = isAsc (x2:xs)


-- has path?
hasPath :: Eq b => [(b, b)] -> b -> b -> Bool
hasPath [] a b = a == b
hasPath xs a b
    | a == b        = True
    | otherwise     =
        let xs' = [(n,m) | (n,m) <- xs, n /= a] in
            or [hasPath xs' m b | (n,m) <- xs, n == a]


main :: IO ()
main = print $ hasPath [(1,2),(2,3),(3,2),(4,3),(4,5)] 4 1
