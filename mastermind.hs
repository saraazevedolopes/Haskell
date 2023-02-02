{- In this file, I have developed the main function /mastermind/ which starts the game, the /getKey/ which 
generates the random 4-digit sequence that is the answer the player wants to obtain, the /getGuess/ which 
checks the validity of the player's attempt and is later used in the /doGuess/. The /doGuess/ function
counts how many correct values are in the correct position and how many correct values are in the wrong 
position, and then is called again in case the player doesn't win or shows a specific message in case the 
player wins, ending the game. -}

mastermind :: IO ()
mastermind = do 
  (n1,n2,n3,n4) <- getKey
  putStrLn "Enter 4 digits:"
  doGuess (n1,n2,n3,n4)
--return ()
{- Note: The return function in Haskell is used to specify the result value of a computation, but its purpose 
in the /mastermind/ function is redundant, since the result of the /doGuess/ computation is already being 
returned in the form of side-effects (printing to the console), which is the main purpose of the function.
Thus, removing the return () has no effect on the behavior of the program, and it can be safely omitted. -}

getKey :: IO (Int,Int,Int,Int)
getKey = (,,,) <$> randomRIO (0,9) <*> randomRIO (0,9) <*> randomRIO (0,9) <*> randomRIO (0,9)
{- Note:
- the <$> operator is used to apply a function to its argument
- the <*> operator is used to apply a function that has multiple arguments
I could also do: getKey = do a <- randomRIO (0,9)
                             b <- randomRIO (0,9)
                             (...)
                             return (a,b,...) -}

getGuess :: IO (Int,Int,Int,Int)
getGuess = do 
  x <- getLine
  if length x /= 4 || not (all isDigit x) 
    then getGuess 
    else let [a,b,c,d] = map read $ x in return (a,b,c,d)
{- Note: map read is used to convert the string x into a list of integers, and then uses pattern matching
to bind each of the four elements of the list to separate variables a, b, c, and d. -}

doGuess :: (Int,Int,Int,Int) -> IO ()
doGuess (n1,n2,n3,n4) = do
  let listaNums = [n1,n2,n3,n4]
  (g1,g2,g3,g4) <- getGuess
  let numsC = sum [if n == g then 1 else 0 | (n,g) <- zip [n1,n2,n3,n4] [g1,g2,g3,g4]] -- correct values and place
  let numsS = sum [if n /= g && g `elem` listaNums then 1 else 0 | (n,g) <- zip [n1,n2,n3,n4] [g1,g2,g3,g4]] -- correct values and wrong place
  if numsC == 4 
    then print "Congratulations, you won!" 
    else print $ "Correct values: " ++ show numsC ++ "   Wrong place values: " ++ show numsS
  unless (numsC == 4) $ doGuess (n1,n2,n3,n4) -- unless function recursively call itself unless the player has won, this allows the player to continue the game

  
