-- You can load this file into the Haskell interpreter. (The command is called "ghci".) When
-- running the interactive shell, use ":help" for a list of useful directives.
-- Modules can be loaded into the interactive environment using the command
-- ":l". Tests can be run from the shell by simply evaluating the appropriate
-- "test" function.
--
-- As always, you are highly encouraged to add your own test cases for each task
-- (which, of course, should not introduce any compilation errors!). If your solution
-- only passes the given test cases but fails for other inputs that satisfy the criteria
-- given in the task descriptions, you will not get full points.

module Assignment where

import Control.Exception

-- **** Task 2.1 ****
--
-- The interpreter below must be changed so that it evaluates in the eager way.
-- Hint: eager evaluation can be forced in Haskell with the "seq" function
--
-- **** Task 2.2 ****
--
-- Try to implement your own version of "seq". It is sufficient if your
-- function works only for the data type "Value". You can use the template
-- of the function "myseq" which is given below. Replace the standard "seq"
-- with your own function in the interpreter.
--
-- Note:
-- If you do both tasks, it is sufficient to only upload the solution for Task 2.2
-- to the submission system, where you replaced all "seq" calls from Task 2.1 with
-- "myseq". We will then grade Task 2.1 by assuming that these are also the
-- positions where you put "seq" before. However, if you cannot completely solve
-- Task 7, then please do not replace your "seq" calls from Task 2.2 anywhere.

type Identifier = String

data Value = NumV Int
  | ClosureV Identifier WAE Env
    deriving (Eq, Show)

type Env = [(Identifier, Value)]

data WAE = Num Int
         | Add WAE WAE
         | Sub WAE WAE
         | Id Identifier
         | With Identifier WAE WAE
         | If0 WAE WAE WAE
         | Fun Identifier WAE
         | App WAE WAE
     deriving (Eq, Show)

interp :: WAE -> Env -> Value

interp (Num n) _ = NumV n

interp (Id i) sc = v where (Just v) = lookup i sc

interp (Add lhs rhs) sc = NumV (n1 + n2) where
  (NumV n1) = interp lhs sc
  (NumV n2) = interp rhs sc

interp (Sub lhs rhs) sc = NumV (n1 - n2) where
  (NumV n1) = interp lhs sc
  (NumV n2) = interp rhs sc

interp (With bound_id named_expr bound_body) sc =
  interp bound_body ((bound_id, exprVal):sc) where
  exprVal = (interp named_expr sc)

interp (If0 cond lhs rhs) sc =
  if condVal == 0 then interp lhs sc else interp rhs sc where
  (NumV condVal) = interp cond sc

interp (Fun bound_id bound_body) sc = ClosureV bound_id bound_body sc

interp (App fun_expr arg_expr) sc =
  interp funBody ((bound_id, argVal):funSc)
  where (ClosureV bound_id funBody funSc) = interp fun_expr sc
        argVal = interp arg_expr sc

-- Forces evaluation of the first argument
myseq :: Value -> Value -> Value
myseq _ b = undefined

-- Test Cases

test5 :: [Bool]
test5 = [
    (interp (Add (Num 2) (Num 5)) []) == (NumV 7),
    (interp (With "x" (Num 5) (Add (Id "x") (Id "x"))) []) == (NumV 10),
    (interp (With "f" (Fun "x" (Add (Id "x") (Id "x")))
                      (App (Id "f") (Num 5))) []) == (NumV 10)
  ]

-- Test Cases for eager evaluation: must generate errors

test5' :: Value
test5' = interp (With "f" (Fun "x" (Num 4)) (App (Id "f") (Id "y"))) []

test5'' :: Value
test5'' = interp (With "x" (Add (Id "y") (Num 3)) (Num 5)) []

test5''' :: Value
test5''' = interp (With "x" (Sub (Id "y") (Num 4)) (Num 5)) []

test6' :: Value
test6' = interp (With "a" (Add (Num 5) (Id "x")) (Num 3)) []

test6'' :: Value
test6'' = interp (With "a" (Sub (Num 6) (Id "x")) (Num 3)) []

-- Do not change the following code.
main :: IO ()
main = do
  print test5
  expect_exception test5'
  expect_exception test5''
  expect_exception test5'''
  expect_exception test6'
  expect_exception test6''
  return ()
  where
    expect_exception val =
      (either (const True) (const False) <$> (try (print val) :: IO (Either SomeException ())) >>= print) :: IO ()
