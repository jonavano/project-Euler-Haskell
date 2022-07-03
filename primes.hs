main = do
    print (primesLessThan2Mill 2000000)
    print (sum (primesLessThan2Mill 2000000))

primes :: [Integer]
primes = 2 : 3 : 5 : 7 : [x | x <- [11 ..], forAll (dividesBy x) (takeWhile (\a -> sqrt (fromIntegral x) >= fromIntegral a) primes)]

forAll :: (a -> Bool) -> [a] -> Bool
forAll bool (x : sx)
  | not (bool x) = forAll bool sx
  | otherwise = False
forAll _ _ = True

primesLessThan2Mill a = takeWhile (< a) primes

dividesBy :: Integral a => a -> a -> Bool
dividesBy input x = input `mod` x == 0
