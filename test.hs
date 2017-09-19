double    x   = x + x
quadruple x   = double (double x)
factorial x   = product [1 .. x]
average   ls  = sum ls `div` length ls

newFN x = b + c
  where
    b = 1
    c = 2

f x y = let     -- f x y not f(x,y)
  z1 = x*x
  z2 = y*y
  z3 = z1 + z2
  in sqrt z3    -- sqrt z3 not sqrt(z3)

lastC  xs = xs !! (length xs - 1)
lastC2 xs = head (reverse xs)
lastC3 xs = last xs

lastC4 xs
  | length xs = 1 = head xs
  | otherwise lastC4 (tail xs)

initC xs = reverse (tail (reverse xs))
