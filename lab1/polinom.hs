evalPolinomial :: Floating a => [a] -> a -> a
evalPolinomial [] _ = 0
evalPolinomial [a_0] _ = a_0
evalPolinomial arr x =
    let len = length arr in
        sum [a_i * (x ** fromIntegral p) | (a_i, p) <- zip arr [len - 1, len - 2..0]]


main = print $ evalPolinomial [2, 1, 5] 3
