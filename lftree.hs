{- Consider the following type to represent a /Leaf Tree/. In these trees, the information is in the nodes (the ends of the 
tree have only a mark – Empty). It is also usual to define trees in which the information is only on the extremities. -}
data LTree a = Tip a | Fork (LTree a) (LTree a)
example1 = Fork (Fork (Tip 1) (Tip 2)) (Fork (Tip 3) (Tip 4)) -- to test code

{- Representation of example1:
         ____Fork____
       |           |
    __Fork__     __Fork__
   |       |    |       |
   1       2    3       4                   -}

{- Define a function that adds the leaves of a /leaf tree/. -}
ltSum :: Num a => LTree a -> a
ltSum (Tip n) = n
ltSum (Fork a b) = ltSum a + ltSum b 

{- Define a function that lists the leaves of a /leaf tree/, from left to on the right. -}
listLT :: LTree a -> [a]
listLT (Tip n) = [n]
listLT (Fork a b) = listLT a ++ listLT b

{- Define a function that calculates the height of a /leaf tree/. -}
ltHeight :: LTree a -> Int
ltHeight (Tip _) = 0
ltHeight (Fork a b) = 1 + max (ltHeight a) (ltHeight b)


{- I will now define the data of a /Full Tree/. In these trees, the information is not only in the nodes, but also in the
leaves (note that the type of information in nodes and leaves does not have to be the same). -}
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show -- remembering /binary tree/ data 
--data LTree a = Tip a | Fork (LTree a) (LTree a) deriving Show -- remembering /leaf tree/ data 

{- Define a function that separates a /full tree/ with information on nodes and leaves in two type trees many different. -}
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf n) = (Empty, Tip n)
splitFTree (No a b c) = (Node a (fst (splitFTree b)) (fst (splitFTree c)), Fork (snd (splitFTree b)) (snd (splitFTree c)))

{- Define a function that whenever the trees are compatible joins them into one /full tree/. -}
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees (Empty) (Tip n) = Just (Leaf n)
joinTrees (Node e l r) (Fork a b) = Just (No e aux aux2)
    where Just aux = joinTrees l a
          Just aux2 = joinTrees r b
joinTrees _ _ = Nothing 

{- Define a function that receive a /Leaf Int/ as input and generates a /Full Tree Int Int/ as output. The new tree 
preserves the value of the leaves and puts into each node is the sum of all the leaves of the tree rooted at that node. -}
conv :: LTree Int -> FTree Int Int
conv (Tip x) = Leaf x
conv (Fork l r) = No (ltSumInt l + ltSumInt r) (conv l) (conv r)

ltSumInt :: LTree Int -> Int
ltSumInt (Tip x) = x
ltSumInt (Fork l r) = ltSumInt l + ltSumInt r

it = conv example1 -- to test the function conv
instance (Show a, Show b) => Show (FTree a b) where
    show (Leaf x) = "Leaf " ++ show x
    show (No x l r) = "No " ++ show x ++ " (" ++ show l ++ ") (" ++ show r ++ ")"
