{- Alvin Han // hana7 // 400119605 // 02/12/17 // Assignment 5 -}

module DefiniteIntegral where

  -- Approximates the definite integral of a given lambda function g using the trapezoidal rule
  definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
  definiteIntegral a b g n                    -- this spits out an error that definiteIntegral _ _ _ _ is unnacounted for but I believe since all variables are used in this case that this is not true?
    | a == b = 0                              -- if the lower and upper bound are identical then the area underneath is 0
    | a /= b = (((g (a+(b-a)/fromIntegral n)+g a)/2)*((b-a)/fromIntegral n)) + definiteIntegral (a+(b-a)/fromIntegral n) b g (n-1) -- trapezoid rule for one segment + the next recursively until the two buonds are identical

  circleArea :: Double -> Double              -- uses integration to find the area of a quadrant of a circle, then multiplies it by four
  circleArea r = 4 * definiteIntegral 0 r (\x->sqrt(r**2-x**2)) 1001

  sphereVolume :: Double -> Double            -- uses volumes of rotation to find the volume of half a sphere then multiples by two
  sphereVolume r = 2 * pi * definiteIntegral 0 r (\x->r**2-x**2) 1001

  {-
  Function: definiteIntegral
  Test Case Number: 1
  Input: definiteIntegral 0 1 (\x->x) 10000
  Expected Output: 0.5
  Actual Output: 0.500000000000088

  Function: definiteIntegral
  Test Case Number: 2
  Input: definiteIntegral (-1) 1 (\x->2.718**x) 10000
  Expected Output: 2.35033
  Actual Output: 2.3503261132212288

  Function: definiteIntegral
  Test Case Number: 3
  Input: definiteIntegral (-9) 12 (\x->sqrt x) 10000
  Expected Output: NaN
  Actual Output: Nan

  Function: circleArea
  Test Case Number: 1
  Input: circleArea 10
  Expected Output: 314.159
  Actual Output: 314.15555226202906

  Function: circleArea
  Test Case Number: 2
  Input: circleArea 123
  Expected Output: 47529.155
  Actual Output: 47528.593501721734

  Function: circleArea
  Test Case Number: 3
  Input: circleArea 9.8
  Expected Output: 301.719
  Actual Output: 301.71499239244883

  Function: sphereVolume
  Test Case Number: 1
  Input: sphereVolume 10
  Expected Output: 4188.79
  Actual Output: 4188.789159680134

  Function: sphereVolume
  Test Case Number: 2
  Input: sphereVolume 123
  Expected Output: 7794781.462
  Actual Output: 7794779.517206393

  Function: sphereVolume
  Test Case Number: 3
  Input: sphereVolume 9.8
  Expected Output: 3942.46
  Actual Output: 3942.454846777609

  Function: definiteIntegral
  Property: abs (definiteIntegral a b (\x->x) 1000 - (-definiteIntegral b a (\x->x) 1000)) <= 0.01
  Actual Test Result: Pass

  Function: definiteIntegral
  Property: abs (someConst*definiteIntegral a b (\x->x) 1001 - definiteIntegral a b (\x->someConst*x) 1000) <= 0.01
  Actual Test Result: Pass

  Function: definiteIntegral
  Property: abs (definiteIntegral a b (\x->someConst) 1001 - someConst*(b-a)) <= 0.01
  Actual Test Result: Pass
  -}
