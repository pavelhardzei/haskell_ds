--tail
collatzHelper :: (Integral a1, Num a2) => a1 -> a2 -> a2
collatzHelper n acc
    | n == 1    = acc
    | even n    = collatzHelper (div n 2) (acc + 1)
    | otherwise = collatzHelper (3 * n + 1) (acc + 1)


collatz :: (Integral a1, Num a2) => a1 -> a2
collatz n = collatzHelper n 1


main :: IO ()
main = print $ collatz 7
