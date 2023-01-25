{- Consider the following type to represent a binary tree. -}
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

{- Example to try the code and its representation: 
        4
       / \
      2   6
     / \ 
    1   3
-}
myBTree = Node 4 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 6 Empty Empty)
{- Throughout the code I will represent left and right by their initials. -}

{- How to calculate the height of a /binary tree/? -}
height :: BTree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

{- How to count the nodes of a /binary tree/? -}
countNodes :: BTree a -> Int
countNodes Empty = 0
countNodes (Node _ l r) = 1 + countNodes l + countNodes r

{- How to count the leaves of a /binary tree/? -}
leaves :: BTree a -> Int
leaves Empty = 0
leaves (Node _ Empty Empty) = 1
leaves (Node _ l r) = leaves l + leaves r

{- How to remove elements from a /binary tree/ from a certain depth? -}
prune :: Int -> BTree a -> BTree a
prune _ Empty = Empty
prune 0 _ = Empty
prune x (Node e l r) = Node e (prune (x - 1) l) (prune (x - 1) r)

{- Define a function that given a path (False corresponds to the left and True to the right) and a tree, it gives the list with the information of 
the nodes through which this path passes. -}
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node e l r) = [e]
path (h:t) (Node e l r) = e : path t (if h then r else l)  

{- How to generate a symmetrical /binary tree/?. -}
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node e l r) = Node e (mirror r) (mirror l)

{- Define a function that has the same functionality as zipWith, but applied to /binary trees/. -}
zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node e l r) (Node a b c) = Node (f e a) (zipWithBT f l b) (zipWithBT f r c)
zipWithBT _ _ _ = Empty

{- Define a function that has the same functionality as unzip, but applied to /binary trees/. -}
unzipBT :: BTree (a,b,c) -> (BTree a,BTree b,BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) l r) = (Node a unzipL1 unzipR1, Node b unzipL2 unzipR2, Node c unzipL3 unzipR3)
    where (unzipL1,unzipL2,unzipL3) = unzipBT l
          (unzipR1,unzipR2,unzipR3) = unzipBT r

{- Define a function that has the same functionality as insert, but applied to /binary trees/. -}
insertBT :: (Ord a) => a -> BTree a -> BTree a
insertBT x Empty = Node x Empty Empty
insertBT x (Node e l r)
  | x == e = Node x l r
  | x < e  = Node e (insertBT x l) r
  | x > e  = Node e l (insertBT x r)

{- Define a function that has the same functionality as se, but applied to /binary trees/. -}
searchBT :: (Ord a) => a -> BTree a -> Bool
searchBT _ Empty = False
searchBT x (Node e l r)
  | x == e = True
  | x < e  = searchBT x l
  | x > e  = searchBT x r

{- Define a function that determines the smallest element of a nonempty /binary search tree/. -}
myminimum :: Ord a => BTree a -> a
myminimum (Node e Empty _) = e
myminimum (Node _ l _) = myminimum l

{- Define a function that determines the biggest element of a nonempty /binary search tree/. -}
mymaximum :: Ord a => BTree a -> a
mymaximum (Node e _ Empty) = e
mymaximum (Node _ _ r) = mymaximum r

{- Define a function that removes the smallest element of a nonempty /binary search tree/. -}
nominimum :: Ord a => BTree a -> BTree a
nominimum (Node e Empty Empty) = Empty
nominimum (Node _ Empty r) = r
nominimum (Node e l r) = Node e (nominimum l) r

{- Define a function that removes the biggest element of a nonempty /binary search tree/. -}
nomaximum :: Ord a => BTree a -> BTree a
nomaximum (Node e l r)
    | isEmpty r = l
    | otherwise = Node e l (nomaximum r)

isEmpty :: BTree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Alternative 
nomaximum2 :: Ord a => BTree a -> BTree a
nomaximum2 (Node e Empty Empty) = Empty
nomaximum2 (Node e l Empty) = l
nomaximum2 (Node e l r) = Node e l (nomaximum2 r)

{- Define a function that simultaneously finds the smallest value and removes it from a nonempty /binary search tree/. -} 
minnomin :: Ord a => BTree a -> (a, BTree a)
minnomin (Node e Empty _) = (e, Empty)
minnomin (Node e l r) = (a, Node e b r)
    where (a,b) = minnomin l

{- Define a function that simultaneously finds the biggest value and removes it from a nonempty /binary search tree/. -} 
maxnomax :: Ord a => BTree a -> (a, BTree a)
maxnomax (Node e _ Empty) = (e, Empty)
maxnomax (Node e l r) = (a, Node e l b)
    where (a,b) = maxnomax r

