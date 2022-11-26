xZipWithHelper :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a] -> [a]
xZipWithHelper op xs ys acc
    | null xs || null ys    = acc
    | otherwise             = xZipWithHelper op (tail xs) (tail ys) (acc ++ [op (head xs) (head ys)])


xZipWith :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
xZipWith op xs ys = xZipWithHelper op xs ys []


main :: IO ()
main = print $ xZipWith (+) [10, 20, 30] [9, 8, 7, 6, 5, 4]