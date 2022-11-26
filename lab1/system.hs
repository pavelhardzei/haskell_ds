
fromDigitsHelper :: Floating t => t -> [t] -> t -> t -> t
fromDigitsHelper n xs p res
    | null xs   = res
    | otherwise = fromDigitsHelper n (tail xs) (p + 1) (res + head xs * n ** p)


fromDigits :: Floating t => t -> [t] -> t
fromDigits n xs = fromDigitsHelper n (reverse xs) 0 0


toDigitsHelper :: Integral a => a -> a -> [a] -> [a]
toDigitsHelper p n res
    | n == 0    = res
    | otherwise = toDigitsHelper p (div n p) (mod n p : res)


toDigits :: Integral a => a -> a -> [a]
toDigits p n = toDigitsHelper p n []


addDigitwiseHelper :: Integral a => a -> [a] -> [a] -> a -> [a] -> [a]
addDigitwiseHelper _ [] [] 0 res = res
addDigitwiseHelper n xs [] rem res = addDigitwiseHelper n xs [0] rem res
addDigitwiseHelper n [] ys rem res = addDigitwiseHelper n [0] ys rem res
addDigitwiseHelper n (x:xs) (y:ys) rem res = addDigitwiseHelper n xs ys (div (rem + x + y) n) (mod (rem + x + y) n : res)


addDigitwise :: Integral a => a -> [a] -> [a] -> [a]
addDigitwise n xs ys = addDigitwiseHelper n (reverse xs) (reverse ys) 0 []


main :: IO ()
-- main = print $ fromDigits 2 [1, 0, 1, 1, 0, 0]
-- main = print $ toDigits 2 44
main = print $ addDigitwise 2 [1, 0, 1, 1, 0, 1] [1, 1, 1]
