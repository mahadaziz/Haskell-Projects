{- Assignment 3
 - Name: Mahad Aziz
 - Date: November 3, 2019
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "azizm17"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving Show


newtype PolyList a = PolyList [a]
  deriving Show


{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: This function computes polyValue by evaluating a Poly at a number n
 -}
polyValue :: Num a => Poly a -> a -> a
polyValue X n = n
polyValue (Coef a) n = a
polyValue (Sum p1 q1) n = (polyValue (p1) n) + (polyValue (q1) n)
polyValue (Prod p1 q1) n = (polyValue (p1) n) * (polyValue (q1) n)

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: This function computes polyListValue by evaluating a polynomial list at a number n
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList []) n = 0
polyListValue (PolyList (x:xs)) n = x + n * polyListValue (PolyList xs) n

{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: This function computes polyListSum by evaluating the sum of two polynomial lists
 -}
polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum p1 (PolyList []) = p1
polyListSum (PolyList []) q1 = q1
polyListSum (PolyList x) (PolyList y) = PolyList (polyListSumAux x y)

polyListSumAux :: (Num a,Eq a) => [a] -> [a] -> [a]
polyListSumAux p1 q1
    | length p1 > length q1 = polyListSumAux p1 (q1++[0])
    | length p1 < length q1 = polyListSumAux (p1++[0]) q1
    | length p1 == length q1 = zipWith (+) p1 q1

{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: This function computes polyListDegree by evaluating the degree of a polynomial list
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Int
polyListDegree (PolyList []) = undefined
polyListDegree (PolyList x) = length x - 1

{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: This function computes polyListProd by evaluating the product of two polynomial lists
 -}
polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList x) (PolyList y) = PolyList (polyListProdAux x y)


polyListProdAux :: (Num a,Eq a) => [a] -> [a] -> [a]
polyListProdAux [] _ = []
polyListProdAux _ [] = []
polyListProdAux [x] [y] = [x*y]
polyListProdAux (x:xs) ys = polyListSumAux (map (*x) ys) (0:(polyListProdAux (xs) ys))


{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: This function computes polyListToPoly by turning a polynomial list into a poly
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList []) = Coef 0
polyListToPoly (PolyList [x]) = Coef x
polyListToPoly (PolyList (x:xs)) = Sum (Coef x) $Prod (X) $polyListToPoly $PolyList xs

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: This function computes polyToPolyList by turning a poly into a polynomial list
 -}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList (Coef 0) = PolyList []
polyToPolyList X = PolyList [0,1]
polyToPolyList (Coef p) = PolyList [p]
polyToPolyList (Sum (p) (q)) = polyListSum (polyToPolyList p) (polyToPolyList q)
polyToPolyList (Prod (p) (q)) = polyListProd (polyToPolyList p) (polyToPolyList q)

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: polyValue
 - - Test Case Number: 1
 - - Input: (Coef 5) 4
 - - Expected Output: 5
 - - Actual Output: 5
 - 
 - - Test Case Number: 2
 - - Input: (Sum (Coef 4) (X)) 2
 - - Expected Output: 6
 - - Actual Output: 6
 - 
 - - Test Case Number: 3
 - - Input: (Prod (X) (Coef 0)) 9
 - - Expected Output: 0
 - - Actual Output: 0
 - - -----------------------------------------------------------------
 - - Function: polyListValue
 - - Test Case Number: 1
 - - Input: (PolyList []) 4
 - - Expected Output: 0
 - - Actual Output: 0
 -
 - - Test Case Number: 2
 - - Input: (PolyList [4,-3,2]) 8
 - - Expected Output: 108
 - - Actual Output: 108
 -
 - - Test Case Number: 3
 - - Input: (PolyList [-9,0,-8,0,1]) (-2)
 - - Expected Output: -25
 - - Actual Output: -25
 - -----------------------------------------------------------------
 - - Function: polyListSum
 - - Test Case Number: 1
 - - Input: (PolyList [1,2,3,5]) (PolyList [])
 - - Expected Output: PolyList [1,2,3,5]
 - - Actual Output: PolyList [1,2,3,5]
 -
 - - Test Case Number: 2
 - - Input: (PolyList [0,-4,0,7]) (PolyList [2,3,-1,6])
 - - Expected Output: PolyList [2,-1,-1,13]
 - - Actual Output: PolyList [2,-1,-1,13]
 -
 - - Test Case Number: 3
 - - Input: (PolyList [-1,-2]) (PolyList [2,3])
 - - Expected Output: PolyList [1,1]
 - - Actual Output: PolyList [1,1]
 - -----------------------------------------------------------------
 - - Function: polyListDegree
 - - Test Case Number: 1
 - - Input: (PolyList [])
 - - Expected Output: undefined
 - - Actual Output: undefined
 -
 - - Test Case Number: 2
 - - Input: (PolyList [1,2,3,4,5])
 - - Expected Output: 4
 - - Actual Output: 4
 -
 - - Test Case Number: 3
 - - Input: (PolyList [1])
 - - Expected Output: 0
 - - Actual Output: 0
 - -----------------------------------------------------------------
 - - Function: polyListProd
 - - Test Case Number: 1
 - - Input: (PolyList [1,-2,3,-4]) (PolyList [])
 - - Expected Output: PolyList []
 - - Actual Output: PolyList []
 -
 - - Test Case Number: 2
 - - Input: (PolyList [-3,8,0,1]) (PolyList [-5])
 - - Expected Output: PolyList [15,-40,0,-5]
 - - Actual Output: PolyList [15,-40,0,-5]
 - 
 - - Test Case Number: 3
 - - Input: (PolyList [7,8,9]) (PolyList [-3,-4,-5])
 - - Expected Output: PolyList [-21,-52,-94,-76,-45]
 - - Actual Output: PolyList [-21,-52,-94,-76,-45]
 - -----------------------------------------------------------------
 - - Function: polyListToPoly
 - - Test Case Number: 1
 - - Input: (PolyList [])
 - - Expected Output: Coef 0
 - - Actual Output: Coef 0
 -
 - - Test Case Number: 2
 - - Input: (PolyList [-6,25,-27,5])
 - - Expected Output: Sum (Coef (-6)) (Prod X (Sum (Coef 25) (Prod X (Sum (Coef (-27)) (Prod X (Coef 5))))))
 - - Actual Output: Sum (Coef (-6)) (Prod X (Sum (Coef 25) (Prod X (Sum (Coef (-27)) (Prod X (Coef 5))))))
 - 
 - - Test Case Number: 3
 - - Input: (PolyList [20,-14,2])
 - - Expected Output: Sum (Coef 20) (Prod X (Sum (Coef (-14)) (Prod X (Coef 2))))
 - - Actual Output: Sum (Coef 20) (Prod X (Sum (Coef (-14)) (Prod X (Coef 2))))
 - -----------------------------------------------------------------
 - - Function: polyToPolyList
 - - Test Case Number: 1
 - - Input: Coef 0
 - - Expected Output: PolyList []
 - - Actual Output: PolyList []
 -
 - - Test Case Number: 2
 - - Input: Sum (Coef 20) (Prod X (Sum (Coef (-14)) (Prod X (Coef 2))))
 - - Expected Output: PolyList [20,-14,2]
 - - Actual Output: PolyList [20,-14,2]
 - 
 - - Test Case Number: 3
 - - Input: Sum (Coef (-6)) (Prod X (Sum (Coef 25) (Prod X (Sum (Coef (-27)) (Prod X (Coef 5))))))
 - - Expected Output: PolyList [-6,25,-27,5]
 - - Actual Output: PolyList [-6,25,-27,5]
 - -----------------------------------------------------------------
 -}
