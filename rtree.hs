import Text.Show.Functions
import Data.Maybe
import Data.Char
import Data.List

{- Consider the following type to represent irregular trees (rose trees). -}
data RTree a = R a [RTree a] deriving (Show, Eq) -- the Show and Eq are needing to test in the terminal functions like prune
exampleTree :: RTree Int
exampleTree = R 1 [R 2 [], R 3 [R 4 [], R 5 []], R 6 [R 7 [], R 8 [R 9 []]]]

{- Example structure of exampleTree:

1
|- 2
|- 3
   |- 4
   |- 5
|- 6
   |- 7
   |- 8
      |- 9
 -}

{- How to add the elements of a /rose tree/? -}
add :: Num a => RTree a -> a
add (R a []) = a
add (R a l) = a + sum (map add l)

{- How to calculate the height of a /rose tree/? -}
height :: RTree a -> Int
height (R a []) = 1
height (R a l) = 1 + maximum (map height l)

{- How to remove elements from a /rose tree/ from a certain depth? -}
prune :: Int -> RTree a -> RTree a
prune 0 (R a l) = R a []
prune x (R a l) = R a (map (prune (x-1)) l)

{- How to generate a symmetrical /rose tree/?. -}
mirror :: RTree a -> RTree a 
mirror (R a l) = R a (map mirror (reverse l))

{- How to calculate all the paths from the root to the leaves of a /rose tree/? -}
paths :: RTree a -> [[a]]
paths (R a []) = [[a]]
paths (R a l) = map (a:) (concatMap paths l)

-- Alternative
paths2 :: RTree a -> [[a]]
paths2 (R a l) = getPaths a l

getPaths :: a -> [RTree a] -> [[a]]
getPaths a [] = [[a]]
getPaths a (l:ls) = map (a:) (paths2 l) ++ getPaths a ls

{- Define the inverse function of the previous function. -}
unpaths :: Eq a => [[a]] -> RTree a
unpaths [] = error "Cannot construct tree from empty list of paths"
unpaths [[a]] = R a []
unpaths (p:ps) = R (head p) (map (unpaths . map tail) $ groupBy (\x y -> head x == head y) ps)

{- How to get the postorder and preorder traversals of a /rose tree/?
Preorder and postorder traversals are defined for binary trees, where each node has a left and right child. In the case of a /rose tree/, there is 
no notion of left and right subtrees, so a traversal order like preorder and postorder is not well defined. With this, the following functions may 
not work correctly for all possible examples. -}
postorder :: RTree a -> [a] 
postorder (R e []) = [e]
postorder (R e es) = concatMap postorder es ++ [e]

preorder :: RTree a -> [a]
preorder (R e []) = [e]
preorder (R e es) = e : concatMap preorder es


-- From now, we'll see data organization

{- To store a phone book of contacts and electronic mail, an Agenda, the following types were defined. There are no repeated names in the agenda and 
for each name there is a list of contacts. -}
data Contact = Home Integer | Work Integer | Cellp Integer | Email String deriving (Show)
type Nome = String
type Agenda = [(Nome, [Contact])] -- As we can see, the way the agenda was defined has similarities with /rose trees/
agenda1 = [("Ana", [Work 67, Email "ccc"]), ("Joana", [Email "aaa"])] -- to test the code 

{- Define a function that, given a name, an email and a calendar, adds this information to the agenda. -}
plusEmail :: Nome -> String -> Agenda -> Agenda
plusEmail n em [] = [(n, [Email em])]
plusEmail n em ((a,(b)):t)
  | n == a = ((a,b ++ [Email em]) : t)
  | otherwise = [(a,b)] ++ plusEmail n em t

{- Define a function that, given a name and a agenda, returns the list of emails associated with that name. If that name does not exist in the 
agenda to be function must return Nothing. -}
seeemails :: Nome -> Agenda -> Maybe [String] 
seeemails n [] = Nothing
seeemails n ((a,b):t) 
  | n == a = Just (map (\ (Email e) -> e) $ filter isEmail b) -- can be rewriten as map (\ (Email e) -> e) (filter isEmail b)
  | otherwise = seeemails n t  
  where isEmail (Email _) = True
        isEmail _ = False

{- Define a function that, given a list of contacts, returns the pair with the list of telephone numbers (both landlines and mobile phones) and the
mailing list, from that list. Implement the function in such a way as to make a single traversal of the list of contacts. -}
query :: [Contact] -> ([Integer],[String])
query = foldr (\c (cellp, emails) -> case c of Cellp n -> (n:cellp,emails)
                                               Home n  -> (n:cellp,emails)
                                               Work n  -> (n:cellp,emails)
                                               Email e -> (cellp,e:emails)) ([],[]) 


{- Consider the following type to represent a hierarchical file system. -}
data FileSystem = File Name | Dir Name [FileSystem] -- As we can see, the way the agenda was defined has similarities with /rose trees/
type Name = String
fs1 = Dir "usr" [Dir "xxx" [File "abc.txt", File "readme", Dir "PF" [File "exemplo.hs"]], Dir "yyy" [], Dir "zzz" [Dir "tmp" [], File "teste.c"]]

{- Define a function that lists the names of all files in a filesystem.-}
files :: FileSystem -> [Name]
files (File n) = [n]
files (Dir _ l) = concatMap files l

-- Alternative without high-order functions
files2 :: FileSystem -> [Name]
files2 (File n) = [n]
files2 (Dir _ l) = filesHelper l

filesHelper :: [FileSystem] -> [Name]
filesHelper [] = []
filesHelper (x:xs) = files2 x ++ filesHelper xs

-- Alternative with 'foldl'
files3 :: FileSystem -> [Name]
files3 (File n) = [n]
files3 (Dir _ l) = foldr (\x acc -> files x ++ acc) [] l

{- Define a function that lists the names of the files of a file system that are in a determined path. If the path is not valid, the function 
must return Nothing. For example, dirFiles fs1 ["usr","xxx"] == Just ["abc.txt","readme"]. -}
dirFiles :: FileSystem -> [Name] -> Maybe [Name]
dirFiles (File n) [] = Just [n]
dirFiles (Dir n files) (h:t)
    | h == n = 
        let results = mapMaybe (\f -> dirFiles f t) files in
            if null results then
                Nothing
            else
                Just $ concat results
    | otherwise = Nothing
dirFiles _ _ = Nothing


{- Consider the following structure for maintaining a dictionary, where the words are arranged alphabetically.
Each tree groups together all words beginning with a given letter. Words are built by descending the tree from
from the root. When a word is complete, the value associated with the last letter is 'Just s', where s is a string 
with the description of the word in question (which corresponds to the path from the root to there). Otherwise 
it's 'Nothing'.
For example, d1 is a dictionary with the portuguese words: cara, caras, caro and carro. -}
d1 = [R ('c',Nothing) [
        R ('a',Nothing) [
          R ('r',Nothing) [
           R ('a',Just "...") [
            R ('s',Just "...") [] ],
           R ('o',Just "...") [],
           R ('r',Nothing) [
            R ('o',Just "...") [] ]]]]]

{- Define a function that, given a word and the information associated with it, adds that entry to the dictionary. if 
the word already exists in the dictionary, updates the information associated with it. -}
type Dictionary = [ RTree (Char, Maybe String) ] -- check line 5 to remember the Rtree data 
myinsert :: String -> String -> Dictionary -> Dictionary
myinsert [] _ d = d
myinsert (w:ws) s [] = [R (w, if null ws then Just s else Nothing) (myinsert ws s [])]
myinsert (w:ws) s (R (c, ms) rt:rs)
    | w == c = R (c, if null ws then Just s else ms) (myinsert ws s rt) : rs
    | w < c = R (w, if null ws then Just s else Nothing) (myinsert ws s []) : R (c, ms) rt : rs
    | otherwise = R (c, ms) rt : myinsert (w:ws) s rs
