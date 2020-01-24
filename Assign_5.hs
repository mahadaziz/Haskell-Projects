{- Assignment 5
 - Name: Mahad Aziz
 - Date: December 1, 2019
 -}
module Assign_5 where

macid :: String
macid = "azizm17"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: This function computes definiteIntegral by taking the upper and lower bounds, function, 
 - and an integer n to calculate the definite integral using the trapezoid rule
 -}
definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double 
definiteIntegral a b g n 
    | a == b = 0
    | otherwise = c / 2 * (sum [2 * g x | x <- [a + c, a + 2*c .. b - c]] + g a + g b)
        where c = (b - a) / fromIntegral n

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: This function computes funH by taking an integer n and calculating the area between the 
 - n root and and the power of n in the interval 0 to 1
 -}
funH :: Integer -> Double
funH n = definiteIntegral 0 1 (\x -> x ** (1 / fromIntegral n)) 1000 - definiteIntegral 0 1 (\x -> x ^ n) 1000

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: This function computes funK by taking a double n and finding the definite integral of the
 - function n to the power of x from the interval -1 to 1
 -}
funK :: Double -> Double
funK n = definiteIntegral (-1) 1 (\x -> n ** x) 1000

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: 1 1 (\x -> x ^ 2) 1000
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 -
 - - Test Case Number: 2
 - - Input: 0 5 (\x -> exp x) 1000
 - - Expected Output: 147.413
 - - Acutal Output: 147.41346621319676
 -
 - - Test Case Number: 3
 - - Input: 5 0 (\x -> exp x) 1000
 - - Expected Output: -147.413
 - - Acutal Output: -147.4134662131999
 - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 1
 - - Input: 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 -
 - - Test Case Number: 2
 - - Input: (-1)
 - - Expected Output: Error
 - - Acutal Output: Exception: Negative exponent
 -
 - - Test Case Number: 3
 - - Input: 1000
 - - Expected Output: 0.9974243711207175
 - - Acutal Output: 0.9974243711207175
 - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 1
 - - Input: 0.01
 - - Expected Output: 21.71270611285835
 - - Acutal Output: 21.71270611285835
 -
 - - Test Case Number: 2
 - - Input: 1000
 - - Expected Output: 144.7669851117239
 - - Acutal Output: 144.7669851117239
 -
 - - Test Case Number: 3
 - - Input: 1
 - - Expected Output: 2.0
 - - Acutal Output: 2.0
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Property: prop1 (a,b,n) = definiteIntegral a a (\x -> x) n == 0
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: funH
 - - Property: prop2 (Positive x) = funH x >= 0
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: funK
 - - Property: prop3 (Positive x) = funK x >= 2
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 -}

