--cubicRoot :: Float -> Float
--cubicRoot (-x) = (-1)*x**(1/3)
--cubicRoot x = x**(1/3)

cubicQ :: Float -> Float -> Float -> Float
cubicQ a b c   = (3 * a * c - b * b) / (9 *(a * a))

cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = (9 * a * b * c - 27 * a * a * d - 2 * b * b * b) / (54 * a * a * a)

cubicS :: Float -> Float -> Float
cubicS q r     = (a ** (1 / 3)) ** (1 / 3) where
  a = r + (q + r * r)

cubicT :: Float -> Float -> Float
cubicT q r     = (a ** (1 / 3)) ** (1 / 3) where
  a = r - (q + r * r)

cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d = let
  s = cubicS (cubicQ a b c) (cubicR a b c d)
  t = cubicT (cubicQ a b c) (cubicR a b c d)
  in  s + t - (b / (3 * a))
