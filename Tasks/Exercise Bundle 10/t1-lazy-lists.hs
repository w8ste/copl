-- You can load this file into the Haskell interpreter. (The command is called "ghci".) When
-- running the interactive shell, use ":help" for a list of useful directives.
-- Modules can be loaded into the interactive environment using the command
-- ":l". Tests can be run from the shell by simply evaluating the appropriate
-- "test" function.
--
-- As always, you are highly encouraged to add your own test cases for each task
-- (which, of course, should not introduce any compilation errors!). If your
-- solution only passes the given test cases but fails for other inputs that
-- satisfy the criteria given in the task descriptions, you will not get full
-- points.

module Assignment where

-- **** Task 1 ****
--
-- Implement the function "distinct", which removes consecutive repeating numbers from a
-- (potentially infinite) list.

distinct :: (Eq a) => [a] -> [a]
distinct = undefined

test1 :: Bool
test1 = take 20 (distinct [x `div` 3 | x <- [1..]]) == take 20 [0..]

test1' :: Bool
test1' = distinct [1, 2, 2, 2, 2, 3, 3, 2, 3] == [1, 2, 3, 2, 3]

-- **** Task 2 ****
--
-- Implement the function "merge", which takes two (potentially infinite) sorted
-- lists and merges them into a new sorted list

merge :: (Ord a) => [a] -> [a] -> [a]
merge = undefined

test2 :: Bool
test2 = merge [3*x | x <- [1..13]] [5*x | x <- [1..7]] ==
        [3, 5, 6, 9, 10, 12, 15, 15, 18, 20, 21, 24, 25, 27, 30, 30, 33, 35, 36, 39]

test2' :: Bool
test2' = take 20 (merge [3*x | x <- [1..]] [5*x | x <- [1..]]) ==
        [3, 5, 6, 9, 10, 12, 15, 15, 18, 20, 21, 24, 25, 27, 30, 30, 33, 35, 36, 39]

-- **** Task 3 ****
--
-- Use "merge" and "distinct" to implement the function "combine2", which must
-- have the same functionality as the function "combine" given below, for
-- the case when the argument lists are strictly increasing. You must reuse
-- above functions "merge" and "distinct".

combine :: (Ord a) => [a] -> [a] -> [a]
combine xs [] = xs
combine [] ys = ys
combine (x:xs) (y:ys)
   | x < y = x : (combine xs (y:ys))
   | x > y = y : (combine (x:xs) ys)
   | otherwise = x : (combine xs ys)

combine2 :: (Ord a) => [a] -> [a] -> [a]
combine2 = undefined

test3 :: Bool
test3 = take 20 (combine [3*x | x <- [1..]] [5*x | x <- [1..]]) ==
        take 20 (combine2 [3*x | x <- [1..]] [5*x | x <- [1..]])

-- **** Task 4 ****
--
-- Implement list comb235, which generates a list of all numbers that have only
-- 2, 3 and 5 as their prime factors, i.e. they can be expressed as
-- multiplication of any number of 2, 3 and 5. The numbers must be generated in
-- the increasing order and not duplicated.
--
-- Hint: Define comb235 as a recursive list similarly as the definition of the
-- Fibonacci sequence  fib = 1:1:(zipWith (+) fib (tail fib))
--
-- For this you should use the functions "combine" and "multLst".

-- Multiplies a list by a number
multLst :: Int -> [Int] -> [Int]
multLst = undefined

comb235 :: [Int]
comb235 = undefined

test4 :: Bool
test4 = take 20 comb235 == [1,2,3,4,5,6,8,9,10,12,15,16,18,20,24,25,27,30,32,36]


-- **** Task 4b ****
--
-- Try to replace "combine" with "combine2" in the implementation of "comb235".
-- Does it work? If it does not, why?

comb235' = 1 : (multLst 2 comb235') `combine` (multLst 3 comb235') `combine` (multLst 5 comb235')


-- **** Task 5 ****
--
-- Imagine you want to implement a function like "distinct" or another function
-- that filters potentially infinite lists using a predefined sort of folding.
-- Which kind of folding ("foldl" or "foldr") should you use? Briefly explain why
-- using the other kind of folding can be problematic.
--
-- Hint: Which kinds of functions can be used to fold an infinite list at all?
-- It might also help to take into account the specific function application
-- behaviour of Haskell.

{-
 Your answer here
 -}

 -- just for testing purposes. Do not change.
main :: IO ()
main = do
  print test1
  print test1'
  print test2
  print test2'
  print test3
  print test4
