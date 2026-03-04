-- Below you are given the familiar interpreter for a language with arithmetic
-- expressions, if-statements, and first-class functions. The interpreter
-- additionally counts the number of executed arithmetic expressions
-- (additions and subtractions). The counter is implemented in a straightforward
-- manner by explicitly passing the counter as an additional parameter and a
-- return value.
--
-- But threading the counter value through the computation like this is
-- cumbersome.
--
-- Thus, implement the function "interp2" that has the same functionality as
-- "interp" but implements the interpreter in a monadic way. The new
-- implementation should use the State monad for automatic propagation of the
-- counter, and the operation "incr" from Task 5 to increment the counter.
--
-- Hint: Have a look at the interpreter in the file "monadic-ae.hs"; this should
-- give you an idea how the result should look like.

data Exp = Num Int
         | Add Exp Exp
         | Sub Exp Exp
         | Id String
         | If0 Exp Exp Exp
         | Fun String Exp
         | App Exp Exp
     deriving (Show, Eq)

data Val = NumV Int
         | FunV String Exp Env
     deriving (Show, Eq)

type Env = [(String, Val)]

-- Straightforward interpreter

interp (Num n) env ct = ((NumV n), ct)

interp (Add l r) env ct =
  let ((NumV lv), ct1) = interp l env ct
      ((NumV rv), ct2) = interp r env ct1
   in (NumV (lv + rv), ct2 + 1)

interp (Sub l r) env ct =
  let ((NumV lv), ct1) = interp l env ct
      ((NumV rv), ct2) = interp r env ct1
   in (NumV (lv - rv), ct2 + 1)

interp (If0 c t e) env ct =
  let ((NumV cv), ct1) = interp c env ct
   in if (cv == 0)
      then interp t env ct1
      else interp e env ct1

interp (Id x) env ct =
  case (lookup x env) of
    (Just v) -> (v, ct)

interp (Fun x e) env ct =
  (FunV x e env, ct)

interp (App fe ae) env ct =
  let (fv, ct1) = interp fe env ct
      (av, ct2) = interp ae env ct1
   in case fv of
        (FunV x b fenv) -> interp b ((x,av):fenv) ct2

-- Monadic interpreter (8 points)

interp2 :: Exp -> Env -> State Int Val
interp2 = undefined

-- Test cases

test6 = let
    prog1 = If0 (Sub (Num 2) (Num 2))
                (Add (Num 1) (Num 4))
                (Num 0)
    prog2 = (App (Fun "f" (Add (App (Id "f") (Num 2)) (App (Id "f") (Num 3))))
                 (Fun "x" (Sub (Id "x") (Num 1))))
    prog3 = (App (Fun "f" (App (Id "f") (Num 3)))
                 (Fun "x" (Sub (Id "x") (Num 1))))
  in [
    interp prog1 [] 0 == runWithCounter (interp2 prog1 []),
    interp prog2 [] 0 == runWithCounter (interp2 prog2 []),
    interp prog3 [] 0 == runWithCounter (interp2 prog3 [])
  ]

