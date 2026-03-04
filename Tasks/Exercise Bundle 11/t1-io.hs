module Assignment where

-- **** Task 1 **** (1 point)
-- 
-- Implement a program that reads a list of strings and outputs *different*
-- words found in the strings in the order of their appearance. Each word
-- should be printed in a separate line. The end of input is denoted by an
-- empty line.
-- 
-- Implement the entire processing in a pure function findWords, and test it
-- by calling printWords from the interactive environment.
-- Exploit lazy evaluation to print out words incrementally: after entering
-- each string newly found words should be immediatelly printed out. Don't
-- use the standard function "nub" in your implementation. If needed implement
-- it yourself.

findWords :: String -> String
findWords str = undefined

printWords = interact findWords

-- Note that the following test does not test correct interaction

test1 = [findWords "aaa ccc bbb\nrrr aaa ttt\nbbb ccc\n\n" == "aaa\nccc\nbbb\nrrr\nttt\n"]

-- **** Task 2 **** (1 point)
-- 
-- Given below is an implementation of while_, which evaluates its first
-- argument as long as it returns True.
-- 
-- Implement the function while which continues based on a test function which
-- is given as first argument. The computation itself (given as second argument)
-- can produce values of any type. while should return a list containing all
-- values which have been returned by evaluating the second argument.

while_ :: IO Bool -> IO ()
while_ b = b >>= \result -> if result then while_ b else return ()

while :: (a -> Bool) -> IO a -> IO [a]
while = undefined

-- **** Task 3 **** (1 point)
-- 
-- Use while and ask to implement a login function. The function should repeat
-- asking for the user's name until the given name is found in validUsers.
-- login should return the name of this valid user.
-- 
-- loginAndGreet demonstrates how to use this function.

validUsers = ["Adam", "Eve", "Josef"]
ask s = putStr (s++" ") >> getLine

login :: IO String
login = undefined

loginAndGreet :: IO ()
loginAndGreet = login >>= \user-> print ("Welcome "++user)

testLogin = login >>= \user-> return (elem user validUsers)

-- **** Task 4 **** (1 point)
-- 
-- Define getString using while and getChar. getString reads characters until
-- it receives '\n' and returns the list of characters (including '\n').

getString :: IO String
getString = undefined

