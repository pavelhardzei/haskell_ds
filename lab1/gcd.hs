gcdFunc :: Integer -> Integer -> Integer
gcdFunc a b
    | a == b    = a
    | a > b     = gcdFunc (a - b) b
    | otherwise = gcdFunc a (b - a)


main :: IO ()
main = print $ gcdFunc 48 32
