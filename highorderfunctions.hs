import Data.List -- to obtain the function 'concat' used in 'concatMap'

{- The function 'applyTwice' takes a function and an argument and applies the function to the argument twice. -}
myapplyTwice :: (a -> a) -> a -> a  
myapplyTwice f x = f (f x) 

{- The function 'flip' takes a function as its argument and returns a new function that is a copy of the original 
function with the first two arguments reversed. -}
myflip1 :: (a -> b -> c) -> (b -> a -> c)  
myflip1 f = g  
    where g x y = f y x

myflip2 :: (a -> b -> c) -> b -> a -> c  
myflip2 f y x = f x y 

{- The function 'map' applies a given function to each element of a list and returns a new list with the results. -}
mymap :: (a -> b) -> [a] -> [b]
mymap _ [] = []
mymap f (x:xs) = f x : mymap f xs

{- The function 'filter' applies a given function to each element of a list and returns a new list with only the 
elements for which the function returned True. -}
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter _ [] = []
myfilter p (x:xs)
  | p x = x : myfilter p xs
  | otherwise = myfilter p xs

{- The function 'concatMap' applies a given function to each element of a list and concatenates the resulting lists. -}
myconcatMap :: (a -> [b]) -> [a] -> [b]
myconcatMap f xs = concat (map f xs)

{- The functions 'foldl' & 'foldr' apply a given binary function to the elements of a list and an accumulator value 
in a left-to-right or right-to-left order, respectively, and returns the final accumulator value (acc). -}
myfoldl :: (b -> a -> b) -> b -> [a] -> b
myfoldl _ acc [] = acc
myfoldl f acc (x:xs) = myfoldl f (f acc x) xs

myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr _ acc [] = acc
myfoldr f acc (x:xs) = f x (myfoldr f acc xs)

{- The functions 'scanl' & 'scanr' are similar to 'foldl' & 'foldr', respectively, but also return a list of all 
intermediate accumulator values. -}
myscanl :: (b -> a -> b) -> b -> [a] -> [b]
myscanl f acc [] = [acc]
myscanl f acc (x:xs) = acc : myscanl f (f acc x) xs

myscanr :: (a -> b -> b) -> b -> [a] -> [b]
myscanr f acc [] = [acc]
myscanr f acc (x:xs) = (acc2) ++ (myscanr f (f x acc) xs)
    where acc2 = myscanr f acc xs

{- The function 'any' takes a predicate function (a function that returns a Boolean value) and a list, and returns 
True if any element of the list satisfies the predicate, and False otherwise. -}
myany :: (a -> Bool) -> [a] -> Bool 
myany f (h:t) = f h || myany f t

{- The function 'zipWith' takes a binary function and two lists, and applies the binary function to the elements of 
the lists pairwise. -}
myzipWith :: (a->b->c) -> [a] -> [b] -> [c]
myzipWith f (h:t) (x:xs) = f h x : myzipWith f t xs
myzipWith _ _ _ = []

{- The functions 'takeWhile' & 'dropWhile' take a function and a list and return the longest prefix of the list for which 
the function returns True and the remaining of the list after the first element that fails to meet the condition 
respectively. -}
mytakeWhile :: (a->Bool) -> [a] -> [a]
mytakeWhile f (h:t) 
 | f h = h : mytakeWhile f t
 | otherwise = []

mydropWhile :: (a->Bool) -> [a] -> [a]
mydropWhile f (h:t)
  | f h = mydropWhile f t
  | otherwise = t

{- The function 'span' takes a predicate function and a list, and returns a pair of lists. The first list contains the 
longest prefix of the input list that satisfies the predicate, while the second list contains the remaining elements of the
input list. -}
myspan1 :: (a-> Bool) -> [a] -> ([a],[a])
myspan1 p l = (mytakeWhile p l, mydropWhile p l)

{- More efficient alternative. -}
myspan2 :: (a-> Bool) -> [a] -> ([a],[a])
myspan2 f (h:t)
  | f h = (h:s1,s2) 
  | otherwise = ([],h:t) 
    where (s1,s2) = myspan2 f t

{- The function 'deleteBy' takes a binary function and a value and a list, and returns a new list with the first occurrence 
of an element that satisfy the binary function with the given value removed. -}
mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy f a (h:t)
 | f a h = t
 | otherwise = h : mydeleteBy f a t 

{- The function 'sortOn' takes a function and a list, and returns the list sorted according to the values returned by the 
function when applied to each element of the list. -}
mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn _ [] = []
mysortOn f (h:t) = insere (h) (mysortOn f t)
  where insere x [] = [x]
        insere x (h:t) = if f x > f h then h:(insere x t) else x:h:t

{- The function '(.)' is a function composition operator. It takes two functions, f and g, and combines them to form a new 
function that applies g to the result of applying f. We can considerate h x = f (g x). -}
mycompose :: (b -> c) -> (a -> b) -> a -> c  
mycompose f g = \x -> f (g x)

{- The function '($)' takes a function and an argument, and applies the function to the argument. -}
myapply :: (a -> b) -> a -> b
myapply f x = f x

{- The function 'uncurry' takes a curried function and a tuple, and applies the curried function to the elements of the 
tuple. -}
myuncurry :: (a -> b -> c) -> (a, b) -> c
myuncurry f (x, y) = f x y

{- The function 'curry' is a way to convert a function that takes a tuple as an argument into a function that takes 
multiple arguments. -}
mycurry :: ((a, b) -> c) -> a -> b -> c
mycurry f x y = f (x, y)

{- The function 'swap' takes a tuple of two elements, (a, b), and returns a new tuple with the elements in the opposite 
order, (b, a). -}
myswap :: (a, b) -> (b, a)
myswap (x, y) = (y, x)

{- The function 'groupBy' stakes a function and a list and groups the elements of the list by the result of 
applying the function to them. -}
mygroupBy :: (Eq b) => (a -> b) -> [a] -> [[a]]
mygroupBy _ [] = []
mygroupBy f (x:xs) = groupBy' f x xs []

groupBy' :: (Eq b) => (a -> b) -> a -> [a] -> [[a]] -> [[a]]
groupBy' _ _ [] acc = acc
groupBy' f x (y:ys) acc
    | f x == f y = groupBy' f x ys (init acc ++ [(last acc) ++ [y]])
    | otherwise = groupBy' f y ys (acc ++ [[y]])

{- The function 'iterate' takes a function and an initial value and applies the function to the initial value repeatedly, 
returning a list of the results. -}
myiterate :: (a -> a) -> a -> [a]
myiterate f x = x : myiterate f (f x)

-- Fibonacci sequence: fibs = iterate (\(a, b) -> (b, a+b)) (0, 1)

{- The function 'cycle' takes a list and returns an infinite list consisting of the elements of the original list repeated 
indefinitely. -}
mycycle :: [a] -> [a]
mycycle xs = ys 
  where ys = xs ++ ys

{- The function 'replicate' takes a number and a value and returns a list with that number of copies of the value. -}
myreplicate :: Int -> a -> [a]
myreplicate n x = take n (cycle [x])

myreplicate2 :: Int -> a -> [a]
myreplicate2 n x = [x | _ <- [1..n]]

-- In here, we have 2 code options for replicate.

{- The function 'sequence' takes a list of actions and returns an action that will perform all of them in order. -}
mysequence :: [[a]] -> [[a]]
mysequence [] = [[]]
mysequence (xs:xss) = [x:ys | x <- xs, ys <- mysequence xss]

