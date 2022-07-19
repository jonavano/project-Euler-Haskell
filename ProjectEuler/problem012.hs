import Data.List (group)
import Data.Time (TimeLocale(amPm))

-- Problem 12: Highly divisible triangular number

main :: IO ()
main = print $ head $ dropWhile (\n -> amountOfFactors n <= 500) triangleNumbers

-------------------------------------------


primes :: [Integer]
primes = 2: 3: 5: 7: [x | x <- [11..], all (\n -> div x n /= 0) (takeWhile (\a -> sqrt (fromIntegral x) > fromIntegral a) primes)]


triangleNumbers = [(n * (n + 1) )`div` 2 | n <- [1..]]

-- triangleNumbersFactorsLength = [(x, amountOfFactors x)| x <- triangleNumbers]


-- highlyDivisibleTriangularNumber n = [(a,b) | (a,b) <- takeWhile (\(c,d) -> d <= n) triangleNumbersFactorsLength, b == n]


primeFactors input = primeFactors' input primes

primeFactors' _ [] = []
primeFactors' input (x:xs)
  | x * x > input = [input]
  | r == 0 = x : primeFactors' q (x:xs)
  | otherwise = primeFactors' input xs
  where (q, r) = quotRem input x


-- countFactors :: [[a]] -> Int
countFactors input = product factors
  where factors = [length x + 1 | x <- input]

amountOfFactors x = countFactors (group (primeFactors x))