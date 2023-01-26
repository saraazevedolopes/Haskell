{- Consider the following type to represent a matrix. -}
type Mat a = [[a]]
matrix1 = [[1,2,3],[0,4,5],[0,0,6]]
matrix2 = [[0,0,1],[0,2,3],[4,5,6]]

{- Representation of matrix1:
    | 1 2 3 |          
    | 0 4 5 |           
    | 0 0 6 |  -}       

{- Define a function that tests whether a matrix is well constructed (i.e., whether all lines have the same dimension). -}
dimOK :: Mat a -> Bool
dimOK (h:t) = all ((==) (length h) . length) t 
-- alternative for the previous line: dimOK (h:t) = all (\x -> length h == length x) t

{- Define a function that calculates the dimension of a matrix. -}
dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, (length . head) m)
-- alternative for the previous line: dimMat m = (length m, (length (head m)))

{- Define a function that adds two matrices. -} 
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))
-- how to test in terminal, example: *Main> zipWMat (+) matrix1 matrix1
{- Note: the variable representing the matrix is not missing. Since its happening nothing to the variable, in the left of equal, we 
can remove it from the left and the right of the equal. With this, the following code is also right: addMat m = zipWith (zipWith (+)) m. 
Variables in other functions will also be omitted throughout this code. -}

{- Define a function that, similarly to what happens with the zipWith function, combines two matrices. Use this function
to define a function that adds two matrices. -}
zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c 
zipWMat = zipWith . zipWith
-- how to test in terminal, example: *Main> zipWMat (+) matrix1 matrix1 

{- Define a function that computes the transpose of a matrix. -}
transpose :: Mat a -> Mat a
transpose xs = map (\n -> map (!! n) xs) [0..(length (head xs) - 1)]

-- Alternative
transpose2 :: Mat a -> Mat a
transpose2 m = [ [(m !! j) !! i | j <- [0..l-1] ] | i <- [0..c-1]]
    where (l,c) = dimMat m

{- Define a function that multiplies matrices. -}
multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [[sum $ zipWith (*) a b | b <- (transpose m2)] | a <- m1]

{- Define a function which checks wether a matrix is ​​upper triangular. -}
triUp :: (Real a) => Mat a -> Bool
triUp m = and [m !! i !! j == 0 | i <- [1..l-1], j <- [0..i-1]]
    where (l,_) = dimMat m

{- Define a function which checks wether a matrix is lower triangular. -}
triLow :: (Real a) => Mat a -> Bool
triLow m = and [x <= y | (x,y) <- zip (concat m) (drop (l+1) (concat m))]
  where (l,_) = dimMat m

{- Define a function that rotates a matrix 90 degrees to the left. -}
rotateLeft :: Mat a -> Mat a
rotateLeft m = [[ map (!! i) m !! j | j <- [0..l-1] ] | i <- [c-1,c-2..0]] 
    where (l,c) = dimMat m

{- Define a function that rotates a matrix 90 degrees to the right. -}
rotateRight :: Mat a -> Mat a
rotateRight m = [[ map (!! i) m !! j | j <- [c-1,c-2..0] ] | i <- [0..l-1]] 
    where (l,c) = dimMat m

{- Define a function that has the effect of a mirror before a given matrix. -}
mirror :: Mat a -> Mat a
mirror m = [[ map (!! j) m !! i | j <- [c-1,c-2..0] ] | i <- [0..l-1]] 
    where (l,c) = dimMat m
