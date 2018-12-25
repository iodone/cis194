-- https://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

module Homework01.Hw01 where 

-- Validating Credit Card Numbers

{-  Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is dou-
bled; the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6]
Add the digits of the doubled values and the undoubled dig-
its from the original number. For example,
[2,3,16,6] becomes 2+3+1+6+6 = 18
Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
If the result equals 0, then the number is valid.
-}

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse $ toDigits n

