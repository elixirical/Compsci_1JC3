-- Alvin Han
-- hana7
-- Assignment 2

module VectorSpace where

  type Vector = (Double, Double, Double)

  vecZero :: Vector
  vecZero = (0,0,0)

  vecScalarProduct :: Double -> Vector -> Vector
  vecScalarProduct someDouble (a,b,c) = (a*someDouble,b*someDouble,c*someDouble)

  vecSum :: Vector -> Vector -> Vector
  vecSum (a,b,c) (x,y,z) = (a+x,b+y,c+z)

  vecMagnitude :: Vector -> Double
  vecMagnitude (a,b,c) = sqrt(a**2+b**2+c**2)

  vecF :: Vector -> [Vector] -> [Double]
  vecF _ [] = []
  vecF x (y:ys) = vecMagnitude(vecSum x (vecScalarProduct (-1) y)):vecF x ys

{-
TEST PLAN

Function: vecScalarProduct
Test: 1
Input: 2 (1,1,1)
Expected Output: (2.0,2.0,2.0)
Actual Output: (2.0,2.0,2.0)

Function: vecScalarProduct
Test: 2
Input: 5 vecZero
Expected Output: (0.0,0.0,0.0)
Actual Output: (0.0,0.0,0.0)

Function: vecScalarProduct
Test: 3
Input: 4.1 (12,(-9),0.5)
Expected Output: (49.2,(-36.9),2.05)
Actual Output: (49.199999999999996,-36.9,2.05)

Function: vecSum
Test: 1
Input: (1,1,1) (2,2,2)
Expected Output: (3.0,3.0,3.0)
Actual Output: (3.0,3.0,3.0)

Function: vecSum
Test: 2
Input: (1,1,1) vecZero
Expected Output: (1.0,1.0,1.0)
Actual Output: (1.0,1.0,1.0)

Function: vecSum
Test: 3
Input: (-2,1,0.9) (0.11,-7,19)
Expected Output: (-1.89,-6,19.9)
Actual Output: (-1.89,-6,19.9)

Function: vecMagnitude
Test: 1
Input: (1,1,1)
Expected Output: 1.7321
Actual Output: 1.7320508075688772

Function: vecMagnitude
Test: 2
Input: vecZero
Expected Output: 0.0
Actual Output: 0.0

Function: vecMagnitude
Test: 3
Input:
Expected Output: 50.4937
Actual Output: 50.49366296873301

Function: vecF
Test: 1
Input: (1,1,1) [(1,1,1),vecZero,(2,3,4)]
Expected Output: [0.0, 1.7321, 3.7416]
Actual Output: [0.0,1.7320508075688772,3.7416573867739413]

Function: vecF
Test: 2
Input: "a" [(1,1,1),vecZero,(2,3,4)]
Expected Output: error
Actual Output: <interactive>:1:6 error:
                  * Couldn't match type `[Char]' with `(Double, Double, Double)'
                    Expected type: Vector
                      Actual type: [Char]
                  * In the first argument of `vecF', namely `"a"'
                    In the expression: vecF "a" [(1, 1, 1), vecZero, (2, 3, 4)]
                    In an equation for `it':
                        it = vecF "a" [(1, 1, 1), vecZero, (2, 3, 4)]

Function: vecF
Test: 3
Input: vecZero [(1,1,1),vecZero,(2,3,4)]
Expected Output: [1.7321, 0.0, 5.3852]
Actual Output: [1.7320508075688772,0.0,5.385164807134504]
-}
