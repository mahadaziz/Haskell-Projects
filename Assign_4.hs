{- Assignment 4
 - Name: Mahad Aziz
 - Date: November 17, 2019
 -}
module Assign_4 where

macid :: String
macid = "azizm17"

data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
 - value
 - -----------------------------------------------------------------
 - Description: This function computes value by evaluating a MathExpr at a number n
 -}
value :: (Floating a, Eq a) => MathExpr a -> a -> a
value X n = n
value (Coef a) n = a
value (Sum ex1 ex2) n = (value ex1 n) + (value ex2 n)
value (Prod ex1 ex2) n = (value ex1 n) * (value ex2 n)
value (Quot ex1 ex2) n = (value ex1 n) / (value ex2 n)
value (Exp ex) n = exp (value ex n)
value (Log ex) n = log (value ex n)

{- -----------------------------------------------------------------
 - simp
 - -----------------------------------------------------------------
 - Description: This function computes simp by taking a MathExpr and simplifying it using the
 - simplification rules
 -}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Sum (Coef 0.0) u) = simp u
simp (Sum u (Coef 0.0)) = simp u
simp (Sum u v) = let
    u' = simp u
    v' = simp v
    in if u' == u && v' == v
        then (Sum u v)
        else simp (Sum u' v')
simp (Prod (Coef 0.0) u) = simp (Coef 0.0)
simp (Prod u (Coef 0.0)) = simp (Coef 0.0)
simp (Prod (Coef 1.0) u) = simp u
simp (Prod u (Coef 1.0)) = simp u
simp (Prod u v) = let
    u' = simp u
    v' = simp v
    in if u' == u && v' == v
        then (Prod u v)
        else simp (Prod u' v')
simp (Quot u (Coef 1.0)) = simp u
simp (Quot u v) = let
    u' = simp u
    v' = simp v
    in if u' == u && v' == v
        then (Quot u v)
        else simp (Quot u' v')
simp (Exp (Coef 0.0)) = simp (Coef 1.0)
simp (Log (Coef 1.0)) = simp (Coef 0.0)
simp u = u

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description: This function computes diff by taking a MathExpr and computing the derivative of that 
 - expression
 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = Coef 1
diff (Coef _) = Coef 0
diff (Sum u v) = Sum (diff u) (diff v)
diff (Prod u v) = Sum (Prod (diff u) v) (Prod u (diff v))
diff (Quot u v) = Quot (Sum (Prod (diff u) v) (Prod (Prod u (Coef (-1))) (diff v))) $Prod v v
diff (Exp u) = Prod (Exp u) (diff u)
diff (Log u) = Quot (diff u) (u)

{- -----------------------------------------------------------------
 - readDiffWrite
 - -----------------------------------------------------------------
 - Description: This function computes readDiffWrite which takes the filepaths to two files and computes
 - the derivatives and then simplifies each of the functions in the first file and writes the second 
 - file with the derivatives
 -}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = do inp <- readFile f
                       writeFile g (parseCalc inp)

parseCalc :: String -> String
parseCalc inp = let
    strings = lines inp
    expr = map read strings :: [MathExpr Double]
    calc = difsim expr
    strings' = map show calc
    in unlines strings'

difsim :: (Floating a,Eq a) => [MathExpr a] -> [MathExpr a]
difsim x = map simp (map diff x)

    
{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: value
 - - Test Case Number: 1
 - - Input: value (Quot X (Coef 0)) 2
 - - Expected Output: Infinity
 - - Acutal Output: Infinity
 -
 - - Test Case Number: 2
 - - Input: value (Log X) 0
 - - Expected Output: -Infinity
 - - Acutal Output: -Infinity
 -
 - - Test Case Number: 3
 - - Input: value (Exp X) (-9999)
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 - - Function: simp
 - - Test Case Number: 1
 - - Input: simp (Exp (Coef 0))
 - - Expected Output: Coef 1.0
 - - Acutal Output: Coef 1.0
 -
 - - Test Case Number: 2
 - - Input: simp (Log (Coef 1))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 -
 - - Test Case Number: 3
 - - Input: simp (Prod (Exp X) (Coef 0))
 - - Expected Output: Coef 0.0
 - - Acutal Output: Coef 0.0
 - -----------------------------------------------------------------
 - - Function: diff
 - - Test Case Number: 1
 - - Input: diff (Prod X X)
 - - Expected Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
 - - Acutal Output: Sum (Prod (Coef 1.0) X) (Prod X (Coef 1.0))
 - 
 - - Test Case Number: 2
 - - Input: diff (Log (Coef 2))
 - - Expected Output: Quot (Coef 0.0) (Coef 2.0)
 - - Acutal Output: Quot (Coef 0.0) (Coef 2.0)
 -
 - - Test Case Number: 3
 - - Input: diff (Sum X (Coef 0.0))
 - - Expected Output: Sum (Coef 1.0) (Coef 0.0)
 - - Acutal Output: Sum (Coef 1.0) (Coef 0.0)
 - -----------------------------------------------------------------
 - - Function: readDiffWrite
 - - Test Case Number: 1
 - - Input: "C:/Users/mahad/Documents/textfile1.txt" "C:/Users/mahad/Documents/textfile2.txt"
 - - File Contents: Quot (Coef 0) X
 - - Expected Output: Quot (Coef 0.0) (Prod X X)
 - - Acutal Output: Quot (Coef 0.0) (Prod X X)
 -
 - - Test Case Number: 2
 - - Input: "C:/Users/mahad/Documents/textfile1.txt" "C:/Users/mahad/Documents/textfile2.txt"
 - - File Contents: Log (Exp X)
 - - Expected Output: Quot (Exp X) (Exp X)
 - - Acutal Output: Quot (Exp X) (Exp X)
 -
 - - Test Case Number: 3
 - - Input: "C:/Users/mahad/Documents/textfile1.txt" "C:/Users/mahad/Documents/textfile2.txt"
 - - File Contents: Sum X (Coef 0)
 - - Expected Output: Coef 1.0
 - - Acutal Output: Coef 1.0
 - -----------------------------------------------------------------
 - - Function: value
 - - Property: prop1 x = value X x == x
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: simp
 - - Property: prop2 x = simp (Prod (Coef 0) (Coef x)) == Coef 0
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 - - Function: diff
 - - Property: prop3 x = diff (Coef x) == Coef 0
 - - Actual Test Result: Pass
 - -----------------------------------------------------------------
 -}

