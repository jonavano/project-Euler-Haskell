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

splitNum :: Int -> [Int]
splitNum n
  | n <= 9 = [n]
  | otherwise = splitNum (n `div` 10) ++ [n `mod` 10]

fat :: [Int] -> [Int]
fat x = concatMap splitNum x


largestPalindromeProduct :: [Int]
largestPalindromeProduct = [x * y | x <- [100 ..999], y <- [100..999], palindrome (splitNum(x * y))]

reallyLongestPalindrome = sort largestPalindromeProduct