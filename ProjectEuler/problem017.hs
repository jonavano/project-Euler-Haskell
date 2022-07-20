

numbersToString n
    | n == 0 = ""
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | n == 10 = "ten"
    | n == 11 = "eleven"
    | n == 12 = "twelve"
    | n == 13 = "thirteen"
    | n == 14 = "fourteen"
    | n == 15 = "fifteen"
    | n == 16 = "sixteen"
    | n == 17 = "seventeen"
    | n == 18 = "eighteen"
    | n == 19 = "nineteen"
    | n < 30 = "twenty " ++ numbersToString (n - 20)
    | n < 40 = "thirty " ++ numbersToString (n - 30)
    | n < 50 = "forty " ++ numbersToString (n - 40)
    | n < 60 = "fifty " ++ numbersToString (n - 50)
    | n < 70 = "sixty " ++ numbersToString (n - 60)
    | n < 80 = "seventy " ++ numbersToString (n - 70)
    | n < 90 = "eighty " ++ numbersToString (n - 80)
    | n < 100 = "ninety " ++ numbersToString (n - 90)
    | n < 1000 = if wholeNumber then numbersToString n1 ++ " hundred"
                    else numbersToString n1 ++ " hundred and " ++ numbersToString (n - (n1 * 100))
    | n == 1000 = "one thousand " ++ numbersToString(n - 1000)
    | otherwise = "owo"
        where   n1 = n `div` 100
                wholeNumber = n `mod` 100 == 0

getLengthNumber n = length $ filter (/=' ') (numbersToString n)

numberLetterCounts = sum [getLengthNumber x | x <- [1..1000]]