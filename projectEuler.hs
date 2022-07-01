import Distribution.Simple.Utils (xargs)
dividesBy3Or5 :: [Integer]
dividesBy3Or5 = [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

multiplesOf3Or5 :: Integer
multiplesOf3Or5 = sum dividesBy3Or5

fib :: Int -> Int
fib a | a <= 1 = 1
fib n = fib(n - 1) + fib(n - 2)

fibSequence = [fib x | x <- [1..], even x]

lessThan4Mill = takeWhile (< 4000000) fibSequence

evenFibonacciNumbers = sum lessThan4Mill