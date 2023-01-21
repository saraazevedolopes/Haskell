import Data.List -- to get the function intercalate 
import Data.Char -- to get the function toUpper  

{- Representing a fraction by Frac and its numerator and denomiator by integers. -}
data Frac = F Integer Integer

{- Define Frac as an instance of the Show class, so that each fraction is presented by (numerator/denominator).-}
instance Show Frac where
    show (F a b) = show a ++ "/" ++ show b
-- With this we can transform F 1 2 into 1/2


{- Consider the following data to represent integer expressions. -}
data Exp a = Const a | Symmetric (Exp a) | Sum (Exp a) (Exp a) | Subtraction (Exp a) (Exp a) | Mult (Exp a) (Exp a)

{- Declare Exp as an instance of Show. -}
instance Show a => Show (Exp a) where
    show (Const a) = show a
    show (Symmetric a) = "(- " ++ show a ++ ")"
    show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
    show (Subtraction a b) = "(" ++ show a ++ " - " ++ show b ++ ")"
    show (Mult a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
--With this we can represent: Const a => "a", Symmetric (Const a) => "(- 2)", Sum (Const a) (Const a) => "(a + a)", ect


{- Consider the following data to represent a date. -}
data Date = D Int Int Int

{- Define Date as an instance of the Show class. -}
instance Show Date where 
    show (D day month year) = intercalate "/" $ map show [day,month,year]
--With this we can represent: D day month year => "day/month/year"
--In a simpler way, the code could also be represented by "show (D day month year) = show day ++ "/" ++ show month ++ "/" ++ show year"

{- Define Extract as an instance of the Show class, so that the presentation of the extract either in order of transaction date with the following, 
and with the following aspect:

Previous balance: 300
---------------------------------------
Date       Description  Credit   Debit
---------------------------------------
5/4/2010   DEPOSIT      2000
10/8/2010  PURCHASE              37,5
1/9/2010   Withdrawal            60
7/1/2011   FEES         100
22/1/2011  ANNUITY               8
---------------------------------------
Current balance: 2294,5
-}

data Movement = Credit Float | Debit Float
data Extract = Ext Float [(Date, String, Movement)]

instance Show Movement where
    show (Credit c) = show c
    show (Debit d) = show d

instance Show Extract where
  show (Ext prevBal trans) = 
    "Previous balance: " ++ show prevBal ++ "\n" ++
    "---------------------------------------\n" ++
    "Date       Description  Credit   Debit\n" ++
    "---------------------------------------\n" ++
    concatMap showTrans trans ++
    "---------------------------------------\n" ++
    "Current balance: " ++ show (prevBal + sum (map movementAmount trans)) ++ "\n"

showTrans :: (Date, String, Movement) -> String
showTrans (date, desc, mov) = 
    show date ++ "   " ++ desc ++ "   " ++
    case mov of
        Credit c -> show c
        Debit d ->  "          " ++ show d ++ "\n"

movementAmount :: (Date, String, Movement) -> Float
movementAmount (_, _, Credit c) = c
movementAmount (_, _, Debit d) = -d

extract = Ext 300 [(D 5 4 2010, "DEPOSIT", Credit 2000),
                   (D 10 8 2010, "PURCHASE", Debit 37.5),
                   (D 1 9 2010, "Withdrawal", Debit 60),
                   (D 7 1 2011, "FEES", Credit 100),
                   (D 22 1 2011, "ANNUITY", Debit 8)]


{- Consider the following data type to represent subsets of real numbers:
data SReals = OO Double Double | CC Double Double | OC Double Double | CO Double Double | Union SReals SReals
- (OO x y) represents the open interval ]x, y[,
- (CC x y) represents the closed interval[x, y],
- (OC x y) represents ]x, y],
- (CO x y) represents [x, y[,
- (Union a b) represents the union of sets.
Define SReals as an instance of the Show class, so that, for example, the presentation of the term:
Uniao (Uniao (OO 4.2 5.5) (OC 3.1 7.0)) (CC (-12.3) 30.0) => ((]4.2,5.5[ U ]3.1,7.0]) U [-12.3,30.0])-}

data SReals = OO Double Double | CC Double Double | OC Double Double | CO Double Double | Union SReals SReals
a1 = Union (Union (OO 4.2 5.5) (OC 3.1 7.0)) (CC (-12.3) 30.0) 

instance Show SReals where
    show (OO a b) = "]" ++ show a ++ "," ++ show b ++ "["
    show (CC a b) = "[" ++ show a ++ "," ++ show b ++ "]"
    show (OC a b) = "]" ++ show a ++ "," ++ show b ++ "]"
    show (CO a b) = "[" ++ show a ++ "," ++ show b ++ "["
    show (Union a b) = "(" ++ show a ++ "U" ++ show b ++ ")"

{- Define BTree, a term associated with binary search trees, as an instance of the Show class so that show a2 produces the following string:
"((* <-3-> *) <-5-> (* <-7-> (* <-9-> *)))"-}
data BTree a = Empty | Node a (BTree a) (BTree a)
a2 = Node 5 (Node 3 Empty Empty) (Node 7 Empty (Node 9 Empty Empty))

instance Show a => Show (BTree a) where 
  show (Empty) = "*"
  show (Node a x y) = "(" ++ show x ++ " <-" ++ show a ++ "-> " ++ show y ++ ")"

{- It was decided to organize a telephone book in a binary search tree (sorted in order alphabetical names). For this, the types of data presented 
below were declared. Define Agenda as an instance of the Show class so that the visualization of the tree resultsin a list of information sorted in 
alphabetical order (with one record per line) and in that the various telephones associated with a name are separated by "/"". -}
type Name = String
type Telephone = Integer
data Agenda = E | N (Name,[Telephone]) Agenda Agenda

instance Show Agenda where
  show E = ""
  show (N (n, t) left right) =
    show left ++ n ++ ": " ++ showTelefones t ++ "\n" ++ show right

showTelefones :: [Telephone] -> String
showTelefones [] = ""
showTelefones [t] = show t
showTelefones (t:ts) = show t ++ "/" ++ showTelefones ts

a3 = N ("Maria", [9313456,8752627]) (N ("Bruna", [2782526,30282719]) E E) E






