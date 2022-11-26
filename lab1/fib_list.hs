generalizedFibonacci :: Num a => [a] -> [a]
generalizedFibonacci xs =
    head xs : generalizedFibonacci (tail xs ++ [sum xs])


main :: IO ()
main = print $ take 10 $ generalizedFibonacci [7, 3, 10, 0]
