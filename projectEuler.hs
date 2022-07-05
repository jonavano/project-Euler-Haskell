import Distribution.Simple.Utils (xargs)
import GHC.Float
import Data.List ( sort )
import Distribution.Simple.Command (OptDescr(BoolOpt))
import Distribution.Parsec.Position (positionCol)

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

---------------------------
-- Problem 10: summation Primes

primesLessThan2Mill a = takeWhile (< a) primes

sumOfPrimes a = sum (primesLessThan2Mill a)

primes :: [Integer]
primes = 2: 3: 5: 7: [x | x <- [11..], forAll (dividesBy x) (takeWhile (\a -> sqrt (fromIntegral x) > fromIntegral a) primes)]


forAll :: (a -> Bool) -> [a] -> Bool
forAll bool (x:sx)
  | not (bool x) = forAll bool sx
  | otherwise = False
forAll _ _ = True

checkPrimality :: Integer -> Bool
checkPrimality x = forAll (dividesBy x) (takeWhile (\a -> sqrt (fromIntegral x) > fromIntegral a) primes)

------------------------------------
-- Problem 16: Power digit sum

-- powerDigitSum :: (Integral a, Floating a) => a -> a
powerDigitSum n = sum (splitNum (round (2 ** n)))

powerDigitSumThousand = powerDigitSum 1000

----
-- Problem 11: Largest product in a grid

grid :: [[Int]]
grid =  [[8, 2, 22, 97, 38, 15, 0, 40, 0, 75, 4, 5, 7, 78, 52, 12, 50, 77, 91, 8],
  [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 4, 56, 62, 0],
  [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 3, 49, 13, 36, 65],
  [52, 70, 95, 23, 4, 60, 11, 42, 69, 24, 68, 56, 1, 32, 56, 71, 37, 2, 36, 91],
  [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
  [24, 47, 32, 60, 99, 3, 45, 2, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
  [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
  [67, 26, 20, 68, 2, 62, 12, 20, 95, 63, 94, 39, 63, 8, 40, 91, 66, 49, 94, 21],
  [24, 55, 58, 5, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
  [21, 36, 23, 9, 75, 0, 76, 44, 20, 45, 35, 14, 0, 61, 33, 97, 34, 31, 33, 95],
  [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 3, 80, 4, 62, 16, 14, 9, 53, 56, 92],
  [16, 39, 5, 42, 96, 35, 31, 47, 55, 58, 88, 24, 0, 17, 54, 24, 36, 29, 85, 57],
  [86, 56, 0, 48, 35, 71, 89, 7, 5, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
  [19, 80, 81, 68, 5, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 4, 89, 55, 40],
  [4, 52, 8, 83, 97, 35, 99, 16, 7, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
  [88, 36, 68, 87, 57, 62, 20, 72, 3, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
  [4, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 8, 46, 29, 32, 40, 62, 76, 36],
  [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 4, 36, 16],
  [20, 73, 35, 29, 78, 31, 90, 1, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 5, 54],
  [1, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 1, 89, 19, 67, 48]
  ]
-- grid2 = [
--   [08, 02, 22, 97 ,38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12,50,77,91,08],
--   [49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],
--   [81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],
--   [52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],
--   [22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],
--   [24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],
--   [32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],
--   [67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],
--   [24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],
--   [21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],
--   [78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],
--   [16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],
--   [86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],
--   [19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],
--   [04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],
--   [88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],
--   [04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],
--   [20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],
--   [20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],
--   [01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]
--   ]


invertGrid :: [[Int]]
invertGrid = [[x   | y <- [0..19], x <- [ (grid !! y) !! z] ] | z <- [0..19]]

invertMatrix :: [[a]] -> [[a]]
invertMatrix matrix = [[x   | y <- [0..(length (matrix !! z) -1 )], x <- [ (matrix !! y) !! z] ] | z <- [0..(length matrix - 1)]]

lPGVertical :: [[Int]] -> Int -> Int
lPGVertical (x:sx) largestProduct | largestProductInSeries 0 4 x > largestProduct = lPGVertical sx (largestProductInSeries 0 4 x)
lPGVertical _ largestProduct = largestProduct

lPGDiagonal :: [[Int]] -> Int -> [[Int]]
lPGDiagonal (x:sx) a = drop a x : lPGDiagonal sx (a + 1)
lPGDiagonal _ _ = []


lPGDiagonalLeft :: [[Int]] -> Int -> [[Int]]
lPGDiagonalLeft (x:sx) a = reverse (drop a (reverse x)) : lPGDiagonalLeft sx (a + 1)
lPGDiagonalLeft _ _ = []

-- lPGDiagonal (x:sx) a = reverse (drop (length x - a) (reverse x)) : lPGDiagonal sx (a + 1)

-- test :: [Num a] => a
-- test :: Int
-- test = lPGVertical grid 0


-----
--problem 20: sum of factorial

fac :: (Integral a) => a -> a
fac n = product [1..n]

-- sumOfFactorial :: Int -> Int
sumOfFactorial n = sum (splitNum (fac n))