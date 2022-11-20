-- simple
multiply :: Num a => [a] -> [a] -> [a]
multiply xs ys = [xs !! 0 * ys !! 0 + xs !! 1 * ys !! 2,
                  xs !! 0 * ys !! 1 + xs !! 1 * ys !! 3,
                  xs !! 2 * ys !! 0 + xs !! 3 * ys !! 2,
                  xs !! 2 * ys !! 1 + xs !! 3 * ys !! 3]


fibMatrix :: (Integral a1, Num a2) => [a2] -> a1 -> [a2]
fibMatrix xs n
    | n == 1    = xs
    | even n    = fibMatrix (multiply xs xs) (div n 2)
    | otherwise = multiply xs $ fibMatrix xs (n - 1)


-- tail
fibMatrixHelper :: (Num a2, Integral a1) => [a2] -> a1 -> [a2] -> [a2]
fibMatrixHelper xs n accs
    | n == 1    = multiply xs accs
    | even n    = fibMatrixHelper (multiply xs xs) (div n 2) accs
    | otherwise = fibMatrixHelper xs (n - 1) $ multiply xs accs


fibMatrixTail :: (Integral a1, Num a2) => [a2] -> a1 -> [a2]
fibMatrixTail xs n = fibMatrixHelper xs n [1, 0, 0, 1]


main :: IO ()
main = print $ fibMatrixTail [0, 1, 1, 1] 4
