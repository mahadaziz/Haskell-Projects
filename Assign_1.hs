{- Assignment 1
 - Name: Mahad Aziz
 - Date: September 29, 2019
 -}
module Assign_1 where

macid :: String
macid = "azizm17"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: This function computes cubicQ using doubles a, b, and c as inputs.
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = ((3 * a * c) - (b ** 2)) / (9 * a ** 2)

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: This function computes cubicR using doubles a, b, c, and d as inputs.
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = ((9 * a * b * c) - (27 * a ** 2 * d) - (2 * b ** 3)) / (54 * a ** 3)

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: This function computes the discriminant of the cubic function using the double values
 - from cubicQ and cubicR.
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = (q ** 3) + (r ** 2)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: This function computes cubicT using the discriminant of the cubic function.
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRoot $r + (sqrt(cubicDisc q r))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: This function computes cubicT using the discriminant of the cubic function.
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRoot $r - (sqrt(cubicDisc q r))

{- -----------------------------------------------------------------
 - cubeRoot
 - -----------------------------------------------------------------
 - Description: This function computes the cubic root of a number given a double value e.
 -}
cubeRoot :: Double -> Double
cubeRoot e 
    | e > 0 =  e ** ((1) / (3))
    | e < 0  = ((e * (-1)) ** ((1) / (3))) * (-1)
    | e == 0 = 0

{- -----------------------------------------------------------------
 - x1
 - -----------------------------------------------------------------
 - Description: This function computes the first real root of the cubic function.
 -}
x1 :: Double -> Double -> Double -> Double -> Double
x1 a b c d = (term1 a b c d) - (b / (3 * a))

{- -----------------------------------------------------------------
 - x2
 - -----------------------------------------------------------------
 - Description: This function computes the second real root of the cubic function.
 -}
x2 :: Double -> Double -> Double -> Double -> Double
x2 a b c d = ((term1 a b c d) / (-2)) - (b / (3 * a)) + (term2 a b c d)

{- -----------------------------------------------------------------
 - x3
 - -----------------------------------------------------------------
 - Description: This function computes the third real root of the cubic function.
 -}
x3 :: Double -> Double -> Double -> Double -> Double
x3 a b c d = ((term1 a b c d) / (-2)) - (b / (3 * a)) - (term2 a b c d)

{- -----------------------------------------------------------------
 - t1
 - -----------------------------------------------------------------
 - Description: This function computes the first term in each of the equations that solve for a real root
 -}
term1 :: Double -> Double -> Double -> Double -> Double
term1 a b c d = (cubicS (cubicQ a b c) (cubicR a b c d)) + (cubicT (cubicQ a b c) (cubicR a b c d))

{- -----------------------------------------------------------------
 - t1
 - -----------------------------------------------------------------
 - Description: This function computes the last term in each of the equations that solve for a real root
 -}
term2 :: Double -> Double -> Double -> Double -> Double
term2 a b c d = (((sqrt(3))/2) * (cubicS (cubicQ a b c) (cubicR a b c d) - cubicT (cubicQ a b c) (cubicR a b c d)))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: This function determines the number of real roots of the cubic function using
 - the discriminant and based on that decision it outputs the real roots of the cubic function.
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d 
    | abs (cubicDisc (cubicQ a b c) (cubicR a b c d)) < 0.0001 = [x1 a b c d , x2 a b c d , x3 a b c d]
    | cubicDisc (cubicQ a b c) (cubicR a b c d) > 0 = [x1 a b c d]
    | cubicDisc (cubicQ a b c) (cubicR a b c d) < 0 = []

{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}

-- TODO Add Test Cases for each of your functions below here
-- Test Cases for cubicRealSolutions:
-- 1 (-8) 12 (-3)
-- 1 5 3 (-9)
-- 1 (-5) 8 (-4)
-- 2 (-6) 6 (-2)
-- 2 (-6) 6 (-1)
-- 1 1 1 (-1)
-- Test Case for cubicQ: 1 2 3
-- Test Case for cubicR: 1 2 3 4
-- Test Case for cubicDisc: 1 2
-- Test Case for cubicS: 1 2, (-3) 2
-- Test Case for cubicT: 1 2, (-3) 2
-- Test Cases for cubeRoot: 1, 0, (-1)
-- Test Case for x1: 1 2 3 4
-- Test Case for x2: 1 2 3 4
-- Test Case for x3: 1 2 3 4
-- Test Case for term1: 1 2 3 4
-- Test Case for term2: 1 2 3 4