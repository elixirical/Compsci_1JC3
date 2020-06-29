{- Alvin Han // 400119605 // 2017.11.03 // Assignment 3 -}

module Polynomial where

  data Poly =
      X
    | Coef Integer
    | Sum Poly Poly
    | Prod Poly Poly
    deriving Show

  polyValue :: Poly -> Integer -> Integer                       -- Takes a polynomial and an integer, and evaluates the former at the latter
  polyValue (Coef g) _   = g
  polyValue (Sum g h) n  = polyValue g n + polyValue h n
  polyValue (Prod g h) n = polyValue g n * polyValue h n
  polyValue (X) n        = n

  polyDegree :: Poly -> Integer                                 -- Takes a Poly and returns its degree
  polyDegree (Sum g h)
    | polyDegree g >= polyDegree h = polyDegree g
    | polyDegree h >  polyDegree g = polyDegree h
  polyDegree (Prod g h)  = polyDegree g + polyDegree h
  polyDegree (Coef g)    = 0
  polyDegree (X)         = 1

  polyParse :: Poly -> Poly                                     -- parses a polynomial and removes unnecessary (Coef 0) and (Coef 1)s
  polyParse (Sum (Coef 0) h)  = polyParse h
  polyParse (Sum g (Coef 0))  = polyParse g
  polyParse (Prod (Coef 1) h) = polyParse h
  polyParse (Prod g (Coef 1)) = polyParse g
  polyParse g = g

  polyDeriv :: Poly -> Poly                                     -- returns the derivative of a given Poly; assumes all terms are formatted to have a coeficient and have a chain of Xs
  polyDeriv (Sum g h)    = polyParse (Sum (polyDeriv g) (polyDeriv h))
  polyDeriv (Prod (Coef g) h) = polyParse (Prod (Coef (g*(polyDegree h))) (polyDeriv h))
  polyDeriv (Prod g (Coef h)) = polyParse (Prod (Coef (h*(polyDegree g))) (polyDeriv g))
  polyDeriv (Prod X h)   = polyParse (Prod X (polyDeriv h))
  polyDeriv (Prod g X)   = polyParse (Prod X (polyDeriv g))
  polyDeriv (Coef g)     = Coef 0
  polyDeriv (X)          = Coef 1

{- Test plan

Function: polyValue
Test: 1
Input: polyValue (Prod (Coef 1) X) 2
Expected Output: 2
Actual Output: 2

Function: polyValue
Test: 2
Input: polyValue (Prod (Coef 10) (Prod X (Prod X X))) 2
Expected Output: 80
Actual Output: 80

Function: polyValue
Test: 3
Input: polyValue (Sum (Coef 5) (Sum (Prod (Coef 7) X) (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 18) (Prod X (Prod X X)))))) 2
Expected Output: 171
Actual Output: 171

Function: polyDegree
Test: 1
Input: polyDegree (Prod (Coef 1) X)
Expected Output: 1
Actual Output: 1

Function: polyDegree
Test: 2
Input: polyDegree (Prod (Coef 1) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X)))))))
Expected Output: 7
Actual Output: 7

Function: polyDegree
Test: 3
Input: polyDegree (Sum (Coef 5) (Sum (Prod (Coef 7) X) (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 18) (Prod X (Prod X X))))))
Expected Output: 3
Actual Output: 3

Function: polyDeriv
Test: 1
Input: polyDeriv (Prod (Coef 1) X)
Expected Output: Coef 1
Actual Output: Coef 1

Function: polyDeriv
Test: 2
Input: polyDeriv (Prod (Coef 1) (Prod X (Prod X (Prod X (Prod X (Prod X (Prod X X)))))))
Expected Output: (Prod (Coef 7) (Prod X (Prod X (Prod X (Prod X (Prod X X)))))))
Actual Output: 80

Function: polyValue
Test: 3
Input: polyDeriv (Sum (Coef 5) (Sum (Prod (Coef 7) X) (Sum (Prod (Coef 2) (Prod X X)) (Prod (Coef 18) (Prod X (Prod X X))))))
Expected Output: Sum (Coef 7) (Sum (Prod (Coef 4) X) (Prod (Coef 54) (Prod X X)))
Actual Output: Sum (Coef 7) (Sum (Prod (Coef 4) X) (Prod (Coef 54) (Prod X X)))

-}
