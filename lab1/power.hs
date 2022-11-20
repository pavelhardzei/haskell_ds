-- simple realization
powerFunc :: (Integral a1, Num a2) => a2 -> a1 -> a2
powerFunc x p
    | p == 1    = x
    | even p    = powerFunc (x * x) (div p 2)
    | otherwise = x * powerFunc x (p - 1)


-- tail recursion
powerFuncHelper :: (Num a2, Integral a1) => a2 -> a1 -> a2 -> a2
powerFuncHelper x p acc
    | p == 1    = x * acc
    | even p    = powerFuncHelper (x * x) (div p 2) acc
    | otherwise = powerFuncHelper x (p - 1) (x * acc)


powerFuncTail :: (Integral a1, Num a2) => a2 -> a1 -> a2
powerFuncTail x p = powerFuncHelper x p 1


main :: IO ()
main = print $ powerFuncTail 2 9
