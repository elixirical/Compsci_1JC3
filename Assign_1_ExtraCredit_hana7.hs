import Data.Complex

cubicQ :: Float -> Float -> Float -> Float
cubicQ a b c   = (3 * a * c - b ^ 2) / (9 *(a ^ 2))

cubicR :: Float -> Float -> Float -> Float -> Float
cubicR a b c d = (9 * a * b * c - 27 * a ^ 2 * d - 2 * b ^ 3) / (54 * a ^ 3)

cubicComplexS :: Float -> Float -> Complex Float
cubicComplexS q r = ((r :+ 0) + (sqrt((q ^ 3 + r ^ 2) :+ 0)))**(1/3)

cubicComplexT :: Float -> Float -> Complex Float
cubicComplexT q r = ((r :+ 0) - (sqrt((q ^ 3 + r ^ 2) :+ 0)))**(1/3)

cubicRealSolution :: Float -> Float -> Float -> Float -> Complex Float
cubicRealSolution a b c d = cubicComplexS (cubicQ a b c) (cubicR a b c d) + cubicComplexT (cubicQ a b c) (cubicR a b c d) - (b / (3 * a) :+ 0)
