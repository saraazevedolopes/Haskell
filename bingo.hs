import System.Random
import System.IO 
import Data.List

{- /bingo/ is a function that randomly draws numbers for the bingo game. Whenever 
a key is pressed, a random number between 1 and 90 is displayed. 
Obviously, repeated numbers cannot be displayed and the program terminates after
90 different numbers are generated. -}

bingo :: IO ()
bingo = do
    nums <- numAccumulator
    mapM_ (\n -> do
        putStrLn (show n)
        hFlush stdout
        getLine) nums
    bingo

numAccumulator :: IO [Int]
numAccumulator = do
    x <- newStdGen
    return $ take 90 $ nub $ randomRs (1, 90) x

{- Notes about the previous code:

    The function 'numAccumulator' is responsible for creating a list of 
random  numbers, which has the following characteristics:
-1st These numbers are not repeated, as the 'nub' function prevents repetitions.
-2nd The list has 90 elements, due to the action of the 'take' function.
-3rd The numbers in the list are between 1 and 90, due to the 'randomRs' function.
-4th The numbers generated are pseudo-random due to the 'newStdGen' generating a 
new random generator using the system's default random number generator.

    The function 'bingo' is responsible for presenting the numbers obtained in 
the lists produced by the 'numAccumulator' function. In other words, the 
'numAccumulator' function generates a list and the 'bingo' function will present 
its 1st element, then the 2nd and so on until the elements of the generated list 
are exhausted. When these are finished, 'numAccumulator' generates a new list and 
the process is repeated. Now, I'll explain why I  decided to used certain functions, 
since the code could have been written in other ways.
-1st The function 'mapM_' is similar to 'map', but it has a different purpose. It 
is used to perform a side-effecting action, such as printing to the screen or 
writing to a file, on each element of a list, and it discards the results. In other 
words, it is used when the result of the function applied to each element is not 
important. In the code provided, 'mapM_' is used because the main goal of the 
function is to print the elements of the list one by one, not to return a new list.
'mapM_' allows us to iterate over the elements of the list and perform a side-effect 
on each element, in this case, printing it to the screen, without having to worry 
about collecting and returning the results. 'map' would have returned a list of IO () 
actions, which is not what is desired in this case.
-2nd 'hFlush stdout' is a function from the 'System.IO' module that 'flushes' the output 
buffer of the specified 'handle', in this case the standard output 'handle' represented 
by the constant 'stdout'. When we write something to a 'handle', such as the standard 
output, the data is not immediately sent to the device or file, but stored in a buffer 
in memory. The data is then sent to the device or file in 'chunks' (pieces), or when the 
buffer is full, or when the 'handle' is closed, or the 'hFlush' function is called. 
The 'hFlush stdout' function forces the immediate sending of the data from the buffer to 
the standard output, ensuring that the data is immediately visible on the screen. -}
