import Data.List

type Polynomial = [Monomial]
type Monomial = (Float,Int)

{- In this example, the second element is the exponent and the first one is the coefficient. -}
pol = [(2,3), (3,4), (5,3), (4,5)]

{- Function that selects the monomials of a given exponent of a polynomial. -}
seld :: Int -> Polynomial -> Polynomial
seld d l = filter (\x -> (snd x) == d) l

{- Function that indicates how many monomials of a given exponent exist. -}
count :: Int -> Polynomial -> Int
count g l = length (filter (\x -> (snd x) == g) l)

{- Function that indicates the degree/exponent of a polynomial. -}
grau :: Polynomial -> Int
grau l = maximum (map snd l)

{- Function that calculates the derivative of a polynomial. -}
deriv :: Polynomial -> Polynomial
deriv ((0,0):t) = [(0,0)] ++ deriv t
deriv l = map (\(c,d) -> (c*(fromIntegral d), d-1)) (filter (\(c,d) -> d > 0) l)

{- Function that replaces the x of a polynomial and calculates the value of the polynomial. -}
replace :: Float -> Polynomial -> Float
replace x p = sum [a*x^b | (a,b) <- p]

{- Function that removes from a polynomial the monomials of zero coefficient. -}
simp :: Polynomial -> Polynomial
simp h = filter (\(x,y) -> x /= 0) h

{- Function that calculates the result of multiplying a monomial by a polynomial. -}
mult :: Monomial -> Polynomial -> Polynomial
mult (x,y) p = map (\(c,e) -> (c*x,y+e)) p
--when you have a variable in the right with no function, you can remove it. So, the code "mult (x,y) = map (\(c,e) -> (c*x,y+e))" also works

{- Function that sorts a polonomial in ascending order of the degrees of its monomials. -}
sortspol :: Polynomial -> Polynomial
sortspol = sortOn (snd)
-- sortspol p = sortOn (snd) p

{- Function that given a polynomial constructs a equivalent polynomial in which several monomials with the same cannot appear grade. In other words,
a function that obtains the polynominal in the canonic form. -} 
canpol [] = []
canpol [m] = [m]
canpol ((x,y):t) = (x',y) : canpol t'
  where (x',t') = foldl (\(a,l) (b,d) -> if y == d then (a+b,l) else (a,(b,d):l)) (x, []) t

{- Function that makes the sum of two polynomials, obtaining a polynomial in the canonical form. -} 
sumpol :: Polynomial -> Polynomial -> Polynomial
sumpol p1 p2 = foldr addMonomial [] (p1 ++ p2)
  where
    addMonomial :: Monomial -> Polynomial -> Polynomial
    addMonomial m [] = [m]
    addMonomial (f,s) ((x,y):xs)
      | s == y = (f+x,s) : xs
      | otherwise = (x,y) : addMonomial (f,s) xs
{- If the two polynomials are in the canonical form, we can use: 
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = canpol $ (++) p1 p2. -}

{- Function that calculates the product of two polynomials. -}
polproduct :: Polynomial -> Polynomial -> Polynomial
polproduct ((x,y):t) pol = map (\(c,e) -> (c*x,y+e)) pol ++ polproduct t pol
polproduct _ _ = []

{- Function that checks whether two polynomials are equivalents. -}
equiv :: Polynomial -> Polynomial -> Bool
equiv p1 p2 = null (simp (sumpol p1 (mult (-1,0) p2)))