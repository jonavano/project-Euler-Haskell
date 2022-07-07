main = do
    print collatzListBaby
    print maximums

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

--------------------------------------

collatz 1 = [1]
collatz n | even n = n : collatz (n `div` 2)
collatz n = n : collatz (3 * n + 1)

collatzListBaby :: [(Int,Int)]
collatzListBaby = [(x, length (collatz x)) | x <- [1..999999]]

maximums :: (Int, Int)
maximums = foldr1 (\(x, x2) (y, y2) ->if x2 >= y2 then (x,x2) else (y,y2)) collatzListBaby