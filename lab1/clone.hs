cloneHelper :: (Num t, Enum t) => t -> [a] -> [a] -> [a]
cloneHelper n xs acc
    | null xs   = acc
    | otherwise = cloneHelper n (tail xs) (acc ++ [head xs | _ <- [1..n]])


clone :: (Num t, Enum t) => t -> [a] -> [a]
clone n xs = cloneHelper n xs []


main :: IO ()
main = print $ clone 3 [1, 2, 3]
