{- Assignment 2
 - Name: Mahad Aziz
 - Date: October 21, 2019
 -}
module Assign_2 where

macid :: String
macid = "azizm17"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: This function computes gaussReal by giving the real part of the gaussian integer
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: This function computes gaussImag by giving the imaginary part of the gaussian integer
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y


{- -----------------------------------------------------------------
 - gausConj
 - -----------------------------------------------------------------
 - Description: This function computes gaussConj by giving the conjugate of the gaussian integer
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (gaussReal g , -1 * gaussImag g)

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: This function computes gaussAdd by giving the sum of two gaussian integers
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = (gaussReal g0 + gaussReal g1 , gaussImag g0 + gaussImag g1) 

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: This function computes gaussMult by giving the product of two gaussian integers
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult (a0,b0) (a1,b1) = (a0 * a1  - b0 * b1 , a0 * b1 + b0 * a1)
{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: This function computes gaussNorm by giving the norm of a given gaussian integer
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = gaussReal g ^ 2 + gaussImag g ^ 2

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: This function computes maxGaussNorm by giving the gaussian integer with the greatest norm
 - given from a list of gaussian integers
 -}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm [] = (0,0)
maxGaussNorm (g:gs) = maxGaussNormAux gs g

{- -----------------------------------------------------------------
 - maxGaussNormAux
 - -----------------------------------------------------------------
 - Description: This function computes maxGaussNormAux by taking a list of gaussian integers and 
 - determining the gaussian integer with the greatest norm.
 -}
maxGaussNormAux :: [GaussianInt] -> GaussianInt -> GaussianInt
maxGaussNormAux gs g
    | gs == [] = g
    | gaussNorm (head gs) <= gaussNorm g = maxGaussNormAux (tail gs) g
    | gaussNorm (head gs) > gaussNorm g = maxGaussNormAux (tail gs) (head gs)



{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: (3,7)
 - - Expected Output: (3,-7)
 - - Actual Output: (3,-7)
 - 
 - - Test Case Number: 2
 - - Input: (21,0) 
 - - Expected Output: (21,0)
 - - Actual Output: (21,0)
 - 
 - - Test Case Number: 3
 - - Input: (-9,-15)
 - - Expected Output: (-9,15)
 - - Actual Output: (-9,15)
 - - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: (2,7) (6,4)
 - - Expected Output: (8,11)
 - - Actual Output: (8,11)
 -
 - - Test Case Number: 2
 - - Input: (7,0) (0,-4)
 - - Expected Output: (7,-4)
 - - Actual Output: (7,-4)
 -
 - - Test Case Number: 3
 - - Input: (-5,-4) (-8,-8)
 - - Expected Output: (-13,-12)
 - - Actual Output: (-13,-12)
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: (1,2) (3,4)
 - - Expected Output: (-5,10)
 - - Actual Output: (-5,10)
 -
 - - Test Case Number: 2
 - - Input: (-7,0) (0,9)
 - - Expected Output: (0,-63)
 - - Actual Output: (0,-63)
 -
 - - Test Case Number: 3
 - - Input: (-2,-5) (-1,-8)
 - - Expected Output: (-38,21)
 - - Actual Output: (-38,21)
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: (3,7)
 - - Expected Output: 58
 - - Actual Output: 58
 -
 - - Test Case Number: 2
 - - Input: (-6,0)
 - - Expected Output: 36
 - - Actual Output: 36
 -
 - - Test Case Number: 3
 - - Input: (2,-4)
 - - Expected Output: 20
 - - Actual Output: 20
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: [(4,8),(-7,9),(5,-2),(-10,-6)]
 - - Expected Output: (-10,-6)
 - - Actual Output: (-10,-6)
 -
 - - Test Case Number: 2
 - - Input: [(4,-6),(6,-4),(-6,4),(4,6)]
 - - Expected Output: (4,-6)
 - - Actual Output: (4,-6)
 - 
 - - Test Case Number: 3
 - - Input: [(1,-4),(0,0),(4,1),(-3,-7)]
 - - Expected Output: (-3,-7)
 - - Actual Output: (-3,-7)
 - -----------------------------------------------------------------
 -}

