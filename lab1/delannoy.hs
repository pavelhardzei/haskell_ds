--simple
delannoySimple :: (Eq a, Num a, Num p) => a -> a -> p
delannoySimple m n
    | m * n == 0    = 1
    | otherwise     = delannoySimple (m - 1) n + delannoySimple m (n - 1) + delannoySimple (m - 1) (n - 1)


main :: IO ()
main = print $ delannoySimple 2 2
