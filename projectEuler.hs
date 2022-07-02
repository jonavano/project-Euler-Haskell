import Distribution.Simple.Utils (xargs)
import GHC.Float
import Data.List ( sort )

dividesBy3Or5 :: [Integer]
dividesBy3Or5 = [x | x <- [1 .. 999], x `mod` 3 == 0 || x `mod` 5 == 0]

multiplesOf3Or5 :: Integer
multiplesOf3Or5 = sum dividesBy3Or5

----

fib :: Int -> Int
fib a | a <= 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibSequence = [fib x | x <- [1 ..]]

evenFib = filter even fibSequence

lessThan4Mill = takeWhile (< 4000000) evenFib

evenFibonacciNumbers = sum lessThan4Mill

---

sumOfSquares = sum [x ** 2 | x <- [1 .. 100]]

squareOfSum = sum [1 .. 100] ** 2

sumSquareDifference = squareOfSum - sumOfSquares

----
dividesBy :: Integral a => a -> a -> Bool
dividesBy input x = input `mod` x == 0

isDivisibleForAll input till = takeWhile (dividesBy input) [1 .. till]

checkDivisibility input till = length (isDivisibleForAll input till) == till

-- smallestMultiple :: (Num a, Enum a) => Int -> a
-- smallestMultiple till = head [x | x <- [1..], checkDivisibility x till]

smallestMultiple' till = head [x | x <- [20, 40 ..], checkDivisibility x till]

----

-- 600851475142

-- notDividesBy :: a=> a -> a -> Bool
notDividesBy :: Integral a => a -> a -> Bool
notDividesBy input divider = not (dividesBy input divider)

isDivisibleForNone 1 = True
isDivisibleForNone 2 = True
isDivisibleForNone x = length (takeWhile (notDividesBy x) [2 .. (x - 1)]) == (x -2)

isDivisibleBy x a | a <= 1 = isDivisibleBy x (a + 1)
isDivisibleBy x a | x == a = x
isDivisibleBy x a | mod x a == 0 = a
isDivisibleBy x a = isDivisibleBy x (a + 1)

isPrime :: Int -> Bool
isPrime 1 = True
isPrime x = isDivisibleBy x 2 == x --length (isDivisibleForNone x) == (x - 1)

-- isPrime' :: [Int] -> Int -> Bool
-- isPrime' _ 1 = True
-- isPrime' (x:sx) a | x >= a = True
-- isPrime' (x:sx) a | mod x a == 0 = False
-- isPrime' (x:sx) a = isPrime' sx a
-- isPrime' _ _ = True

listPrimes :: [Int]
listPrimes = 1 : 2 : [x | x <- [3 ..], isPrime x]

-- largestPrimeFactor :: [Int] -> int -> [Int]
largestPrimeFactor :: Integral t => [t] -> t -> [t]
largestPrimeFactor (x : sx) factor | x > factor = []
largestPrimeFactor (x : sx) factor | dividesBy factor x = x : largestPrimeFactor sx (div factor x)
largestPrimeFactor (x : sx) factor = largestPrimeFactor sx factor
largestPrimeFactor [] _ = []

----
--Problem 4 largest palindrome

palindrome :: [Int] -> Bool
palindrome [x] = True
palindrome (x : sx) | x /= last sx = False
palindrome (x : sx) = palindrome (init sx)
palindrome _ = True

-- splitNum :: Int -> [Int]
splitNum :: Integral a => a -> [a]
splitNum n
  | n <= 9 = [n]
  | otherwise = splitNum (n `div` 10) ++ [n `mod` 10]

fat :: [Int] -> [Int]
fat x = concatMap splitNum x


largestPalindromeProduct :: [Int]
largestPalindromeProduct = [x * y | x <- [100 ..999], y <- [100..999], palindrome (splitNum(x * y))]

reallyLongestPalindrome = sort largestPalindromeProduct

---------
--Problem 7: 10001st prime
prime10001 = listPrimes !! 10001

---
--Problem 9: Special Pythagorean triplet
-- the easiest way of finding an higher Pythagorean triplet, is to multiply the original tripled
-- a**2 + b**2 = c**2 = (3,4,5)
multiplicationFactor = 500/5

pythagoreanTriplet = [3,4,5]

-- specialTriplet :: Integer -> [Integer]
specialTriplet a = map (*a) pythagoreanTriplet

productOfList :: Num a => [a] -> a
productOfList (x:sx) = x * productOfList sx
productOfList _ = 1

-- isSum1000 :: Ord a => [Int] -> Int -> [Int]
-- isSum1000 :: [[GHC.Types.Any]] -> [GHC.Types.Any] -> [[GHC.Types.Any]]
-- isSum1000 :: [Integer] -> Integer -> [Integer] Integer
-- isSum1000 :: [Integer] -> Integer -> ([Integer], Integer)
isSum1000 :: [Double] -> Double -> ([Double], Double)
isSum1000 _ a | a <= 0 = ([], 0)
isSum1000 (x:sx) a | sum (x:sx) >= 1000 = (x:sx, a)
isSum1000 (x:sx) a | sum (x:sx) >= 900 = isSum1000 (specialTriplet (a+0.01)) (a+0.01)
isSum1000 (x:sx) a = isSum1000 (specialTriplet (a+1)) (a+1)
isSum1000 _ _ = ([], 0)

-- This was the correct solution, and didn't take long either
-- Ignore the rest of the code :)
isPythagoras = [(x, y, z) | x <- [1..500], y <- [1..500], z <- [1..500], x + y + z == 1000, x**2 + y**2 == z**2]


--------
-- Problem 8: largest product in series
-- largestProductInSeries 0 13 bigNumberList

bigNumber :: Integer
bigNumber = 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450
bigNumberList = splitNum bigNumber

largestProductInSeries highestProduct lengthSeries (x:sx) | productOfList (take lengthSeries (x:sx)) > highestProduct =   largestProductInSeries (productOfList (take lengthSeries (x:sx))) lengthSeries sx
largestProductInSeries highestProduct lengthSeries (x:sx) = largestProductInSeries highestProduct lengthSeries sx
largestProductInSeries highestProduct _ _ = highestProduct