isPerfect :: Integral a => a -> Bool
isPerfect num =
    let divisors = [x | x <- [1..div num 2], rem num x == 0]
    in sum divisors == num


main :: IO ()
main = print $ isPerfect 36
