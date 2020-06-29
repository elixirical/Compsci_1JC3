cubicRoot :: Float -> Float
cubicRoot x = if x >= 0
  then x**(1/3)
  else -((-x)**(1/3))

cubicQ :: Float -> Float -> Float -> Float
cubicQ a b c   = (3 * a * c - b ^ 2) / (9 * a ^ 2)

cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = (9 * a * b * c - 27 * a ^ 2 * d - 2 * b ^ 3) / (54 * a ^ 3)

cubicS :: Float -> Float -> Float
cubicS q r     = cubicRoot a where
  a = r + (q ^ 3 + r ^ 2)**(1/2)

cubicT :: Float -> Float -> Float
cubicT q r     = cubicRoot a where
  a = r - (q ^ 3 + r ^ 2)**(1/2)

cubicRealSolution :: Float -> Float -> Float -> Float -> Float
cubicRealSolution a b c d = let
    s = cubicS (cubicQ a b c) (cubicR a b c d)
    t = cubicT (cubicQ a b c) (cubicR a b c d)
  in (s + t - (b / (3 * a)))
